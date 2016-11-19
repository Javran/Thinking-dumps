{-# LANGUAGE
    ScopedTypeVariables
  , NoMonomorphismRestriction
  , TypeOperators
  #-}
module StreamProcessor where

import qualified Control.Category as Cat
import Control.Arrow

{-# ANN module "HLint: ignore Use const" #-}

{-
  a stream processor maps a stream of input messages
  into a stream of output messages, but is represented
  by an abstract data type.
-}
data SP a b
  = Put b (SP a b)
  | Get (a -> SP a b)

put :: b -> SP a b -> SP a b
put = Put

get :: (a -> SP a b) -> SP a b
get = Get

spArr :: (a -> b) -> SP a b
spArr f = sp'
  where
    -- get one value, apply "f", put it back, and repeat
    sp' = Get (\x -> Put (f x) sp')

-- I think the idea is we want to execute "Put" commands eagerly
-- so that the stream doesn't get stuck
-- also for a stream processor "SP a b",
-- it accepts "a"s on input channel and outputs "b"s on output channel.
-- they doesn't interleave in general, so there is no risk
-- taking "Put x (Get f)" apart and perform actions separately.
spCompose :: SP a b -> SP b c -> SP a c
spCompose sp1 sp2 = case sp2 of
    Put c sp2' ->
        -- sp1 >>> put c >>> sp2'
        -- is the same as:
        -- put c >>> (sp1 >>> sp2')
        -- "c" is put to the final output stream,
        -- so the order is always first "c" and then whatever "sp2" produces.
        Put c (sp1 `spCompose` sp2')
    Get f2 -> case sp1 of
        Put b sp1' ->
            -- pair of Get and Put
            sp1' `spCompose` f2 b
        Get f1 ->
            -- the original impl was:
            -- "Get (\a -> f1 a `spCompose` Get f2)"
            -- but note that "Get f2" is just "sp2" .. so let's do sharing
            -- also note that this is a case where the structure is not getting "smaller"
            Get (\a -> f1 a `spCompose` sp2)

{-
  all we want to do is to deal with "fst" part of the input and apply the arrow to it.
  however, the input also have "snd" part which we are not interested in.
  but still, we need to pass around that data, which is done by maintaining a buffer
  (the first argument to "bypass")
-}
spFirst :: forall a b d . SP a b -> SP (a,d) (b,d)
spFirst = bypass []
  where
    bypass :: [d] -> SP a b -> SP (a,d) (b,d)
    bypass ds (Get f) =
        -- doing "Get" on a "larger" structure
        -- we might receive "d" in the process,
        -- in which case we put that into our buffer
        Get (\(b,d) -> bypass (ds ++ [d]) (f b))
    bypass (d:ds) (Put b sp) =
        -- doing "Put" on a "larger" structure
        -- note that "f" only knows about "fst" part of the input,
        -- we can't construct an arbitrary value oif "d" out of no where,
        -- but we can find some on our buffer
        Put (b,d) (bypass ds sp)
    bypass [] (Put b sp) =
        -- doing "Put" on a larger structure
        -- but this time the buffer is empty.
        -- what we do is to wait for one to come using "Get"
        -- and then do our job.
        --
        -- About the correctness of ignoring one part of the input:
        -- by using scoped type variables, we can explicitly
        -- say that we are accepting something of type (a,d) as the input
        -- but in the output we are going to have something of type (b,d)
        -- and that will be what we have wanted to output.
        Get (\(_ :: a,d :: d) -> Put (b,d) (bypass [] sp))

instance Cat.Category SP where
    id = spArr id
    (.) = flip spCompose

instance Arrow SP where
    arr = spArr
    first = spFirst

instance ArrowZero SP where
    zeroArrow = Get (\_ -> zeroArrow)

{-
  note that even the implementation of "<+>" appears to be symmetric syntactically,
  it is not. Imagine how "(Put v1 sp1) <+> (Put v2 sp2)" will be handled:
  "v1" will always appear in front of "v2", because we pattern match on the first
  argument first. so there are multiple possible implementations of "<+>" and we are
  implementing the left-biased one.

  now what "(f <+> g)" does is to copy the input and feed it to 2 processors,
  and then interleave whatever comes out from both processors, with "Put" conflicts
  resolved by always bias to the left one.
-}
instance ArrowPlus SP where
    -- pushing "Put" out
    (Put b sp1) <+> sp2 = Put b (sp1 <+> sp2)
    sp1 <+> (Put b sp2) = Put b (sp1 <+> sp2)
    (Get f1) <+> (Get f2) = Get (\a -> f1 a <+> f2 a)

fibs :: SP a Integer
fibs = put 0 fibs'
  where
    fibs' = put 1 (liftA2 (+) fibs fibs')

{-
  repeat a value infinitely.
  example:
  - take 10 $ runSP (repeatS 1) []
  - take 10 $ runSP (repeatS 1 >>> arr (+1)) []
  - take 10 $ runSP (repeatS (2,3) >>> first (arr (+ 1))) []
  - take 10 $ runSP (repeatS (2,3) >>> second (arr (+ 1))) []
-}
repeatS :: a -> SP () a
repeatS c = put c (repeatS c)

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

runSP :: SP a b -> [a] -> [b]
runSP ar inp = case ar of
    Get f -> case inp of
        [] -> error "input exhausted"
        (x:xs) -> runSP (f x) xs
    Put c ar' -> c : runSP ar' inp

{-
  turns out in order to "drive" the stream,
  we need a source of infinite inputs,
  even if the input does not offer too much info:

  the following doesn't work:
  - take 10 (runSP f undefined)

  but the following two are working:
  - take 10 (runSP f (repeat ()))
  - take 10 (runSP f (repeat undefined))

  the first working one should be preferred for the obvious reason,
  and the second one is just to show that the stream does not look at
  the value itself at all (otherwise the program would evaluate to bottom)

  relevant: http://stackoverflow.com/q/40618246/315302
  I was thinking which of the following should be used as the input type.
  and it turns out "forall a. a" and "()" are possible alternatives,
  and "Void" should not be used for purposes like this because otherwise
  function user cannot create any "legal" term of that type.
-}

spLeft :: forall a b c. SP a b -> SP (Either a c) (Either b c)
spLeft sp = case sp of
    Put b sp' ->
        -- trivial case, just wrap "b" around and recursively call spLeft
        Put (Left b) (spLeft sp')
    Get f ->
        Get (\(z :: Either a c) -> case z of
                 Left a -> spLeft (f a)
                 Right c ->
                     -- on a "Right" value, what can we do is just
                     -- put it on output and keep getting the next one
                     Put (Right c) (spLeft sp))

instance ArrowChoice SP where
    left = spLeft

{-
  now we have some interpretations of some Arrow operators:

  (+++) :: b `SP` c -> b' `SP` c' -> (Either b b') `SP` (Either c c')

  "f +++ g" is like using one channel to implement two channels,
  by tagging input as "Left" or "Right" and dispatching them to "f" or "g" accordingly.

  (|||) :: b `SP` d -> c `SP` d -> (Either b c) `SP` d

  "f ||| g" unifies "two channels" into one by removing tags.
  the type system ensures that two channels are having compatible outputs.

-}

-- TODO: this type signature is not making sense to me:
-- where does "any1" and "any2" come from if the arguments "f" and "g"
-- are just meant for handling one input and yielding one output?
(|&|) :: (ArrowChoice arr, ArrowPlus arr)
      => Either (Either a any1) (Either any2 b) `arr` c
      -> Either (Either a any1) (Either any2 b) `arr` c
      -> Either a b `arr` c
f |&| g = (arr Left +++ arr Right) >>> (f <+> g)

justLeft :: (ArrowChoice arr, ArrowZero arr) => Either a b `arr` a
justRight :: (ArrowChoice arr, ArrowZero arr) => Either a b `arr` b

-- "justLeft" picks "Left _" elements and consumes/removes "Right _" elements
-- "justRight" does a similar job.
justLeft = arr id ||| zeroArrow
justRight = zeroArrow ||| arr id

dyn :: SP (Either (SP a b) a) b
dyn = dynLoop zeroArrow
  where
    -- dynLoop <inner processor>
    -- TODO: it's a bit unclear about the relation between dynLoop <x> and <x>
    -- the stream processor initially does nothing but
    -- keep getting inputs.
    -- the input could be of two types: "SP a b" or "a"
    -- + when it receives "SP a b" from its input, it changes its "inner processor"
    --   to the processor just received.
    -- + when it receives "a", it feeds the input to its "inner processor"
    --   which then takes over the control
    dynLoop (Put b sp) = Put b (dynLoop sp)
    dynLoop (Get f) = Get $ \z ->
        case z of
            Right a -> dynLoop (f a)
            Left sp -> dynLoop sp
