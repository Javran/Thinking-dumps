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
spFirst :: SP a b -> SP (a,d) (b,d)
spFirst = bypass []
  where
    bypass :: [d] -> SP a b -> SP (a,d) (b,d)
    bypass ds (Get f) =
        -- doing "Get" on a "larger" structure
        -- we might receive "d" in the process,
        -- in which case we put that into our buffer
        Get (\(b,d) -> bypass (ds ++ [d]) (f b))
    bypass (d:ds) (Put c sp) =
        -- doing "Put" on a "larger" structure
        -- note that "f" only knows about "fst" part of the input,
        -- we can't construct an arbitrary value oif "d" out of no where,
        -- but we can find some on our buffer
        Put (c,d) (bypass ds sp)
    bypass [] (Put c sp) =
        -- doing "Put" on a larger structure
        -- but this time the buffer is empty.
        -- what we do is to wait for one to come using "Get"
        -- and then do our job.
        -- (TODO: I'm still in doubt about why we ignore the "fst" part of the input)
        Get (\(_,d) -> Put (c,d) (bypass [] sp))

instance Cat.Category SP where
    id = spArr id
    (.) = flip spCompose

instance Arrow SP where
    arr = spArr
    first = spFirst

instance ArrowZero SP where
    zeroArrow = Get (\_ -> zeroArrow)

instance ArrowPlus SP where
    -- pushing "Put" out
    (Put b sp1) <+> sp2 = Put b (sp1 <+> sp2)
    sp1 <+> (Put b sp2) = Put b (sp1 <+> sp2)
    (Get f1) <+> (Get f2) = Get (\a -> f1 a <+> f2 a)

fibs :: SP a Integer
fibs = put 0 fibs'
  where
    fibs' = put 1 (liftA2 (+) fibs fibs')

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

runSP :: SP a b -> [a] -> [b]
runSP ar inp = case ar of
    Get f -> case inp of
        [] -> error "input exhausted"
        (x:xs) -> runSP (f x) xs
    Put c ar' -> c : runSP ar' inp

-- TODO: something is not quite working:
-- try "take 10 (runSP f undefined)", as "fibs"
-- doesn't really use any input, this should work. (but it's not working
-- after "[0,1" is printed)
