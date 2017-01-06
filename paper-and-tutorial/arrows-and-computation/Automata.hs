{-# LANGUAGE Arrows #-}
module Automata where

import qualified Control.Category as Cat
import Control.Arrow
import Data.Function
import Common

-- an automaton accepts an input, gives an output
-- and changes its own state (returns a changed instance of itself)
-- note that the output also contains "Auto i o",
-- this could cause some confusion if one is not careful and returns
-- a wrong automaton as output.
newtype Auto i o = Auto (i -> (o, Auto i o))

arrAuto :: (i -> o) -> Auto i o
arrAuto f = fix $ \auto -> Auto (\i -> (f i, auto))

-- composing two automata, this is achieved by threading the input
-- through two argument automata.
-- however, one needs to be really careful about what should be the output
-- automata: both "composeAuto f g" and "composeAuto f' g'" fits
-- but only the latter is the correct implementation.
-- so here the type is not sophisticated enough to prevent this kind of error.
composeAuto :: Auto a b -> Auto b c -> Auto a c
composeAuto (Auto f) (Auto g) =
    Auto (\i -> let (b,f') = f i
                    (c,g') = g b
                in (c,f' `composeAuto` g'))

instance Cat.Category Auto where
    id = arrAuto id
    g . f = f `composeAuto` g

instance Arrow Auto where
    arr = arrAuto
    first (Auto f) =
        Auto (\(a,b) ->
              let (c,f') = f a in ((c,b), first f'))

instance ArrowLoop Auto where
    loop (Auto f) = Auto $ \ b ->
        -- basically, we need to apply something to "f" and keep going,
        -- so try "f (b,undefined)" first and see the resulting type,
        -- from which we can replace "undefined" with the proper thing from function's
        -- output.
        -- TODO: still, I have no idea how to make sense of this,
        -- but this strategy works for me.
        let (~(c,d), f') = f (b,d) in (c, loop f')

instance ArrowChoice Auto where
    left (Auto f) = fix $ \ar ->
        Auto $ \ebd -> case ebd of
            Left b ->
                let (c,a') = f b
                in (Left c, left a')
            Right d -> (Right d, ar)

    (Auto l) +++ (Auto r) = Auto $ \v -> case v of
        Left x -> let (y,a) = l x in (Left y, a +++ Auto r)
        Right x -> let (y,a) = r x in (Right y, Auto l +++ a)

{-
exercise 8:
-}
testLHS h = composeAuto (arr untag) h
  where
    untag (Left x) = x
    untag (Right y) = y

-- I'm not entirely sure why ArrowCircuit has to imply ArrowLoop,
-- but this might be just for convenient concerns
class ArrowLoop a => ArrowCircuit a where
    delay :: b -> a b b

{-
  links that could be helpful:

  - http://stackoverflow.com/q/6976944/315302
  - http://stackoverflow.com/q/9856342/315302

  think about "loop (\((),xs) -> (xs, 1:xs)) ()" ?
  - tracking the "snd" part, we see it evolves as "xs -> 1 : xs"
  - the knot is tied by "loop", leaving a function that takes "()" and
    produces an infinite stream of "1"s
-}

-- a generalization of the original version
-- "ArrowLoop" constraint is reduntant because "ArrowCircuit" implies it.
-- but we choose to write this out anyway.
-- TODO: would be interesting to know how this is translated into arrow combinators
counter :: (ArrowLoop a, ArrowCircuit a, Enum e) => a Bool e
counter = proc reset -> do
    -- "reset" introduced as a way to refer to the input to this circuit.

    -- note that "next" is defined after the use site, and this recursive use
    -- needs "ArrowLoop".
    rec output <- returnA -< if reset then zero else next
        next <- delay zero -< succ output
    returnA -< output
  where
    zero = toEnum 0

-- TODO: working on a translation, refering to: https://www.haskell.org/arrows/sugar.html
bind :: Arrow a => a b c -> a (b,c) d -> a b d
e `bind` f = proc b -> do
    c <- e -< b
    d <- f -< (b,c)
    returnA -< d
{- or equivalently:

e `bind` f = (returnA &&& e) >>> f


- basically "returnA" for keeping the input, and the input is also splitted
  and fed to "e" to produce something of "c", which forms the full input for "f"

- don't deny it, this is a bad idea not to write parentheses out explicitly

-}

-- TODO: what to say about variable shadowing?
-- "output" and "next" are originally introduced by "rec",
-- but here the introduction happens in "proc",
-- if we choose to remove "output <-" and "next <-", will it still be correct?
counter2 :: (ArrowLoop a, ArrowCircuit a, Enum e) => a Bool e
counter2 = returnA &&&
           loop (proc (reset,(output,next)) -> do {
                     output' <- returnA -< if reset then zero else next;
                     next' <- delay zero -< succ output;
                     returnA -< (output',(output',next'))}) >>>
           proc (_,output) -> returnA -< output
  where
    zero = toEnum 0

instance ArrowCircuit Auto where
    delay b = Auto $ \ b' -> (b, delay b')

runAuto :: Auto i o -> [i] -> [o]
runAuto (Auto f) xs = case xs of
    [] -> []
    (y:ys)
        | (o, auto') <- f y -> o : runAuto auto' ys

{-
exercise 6: show extensionality axiom fails for the definition of ArrowApply in the paper:

instance ArrowApply Auto where
    app = arr (\(Auto f, x) -> fst (f x))

LHS:

mkPair f >>> app
=> arr (\c -> (f,c)) >>> arr (\(Auto g, x) -> fst (g x))
=> arr (\c -> let (Auto g, x) = (f,c) in fst (g x))
=> arr (\c -> let Auto g = f in fst (g c)) (now let f = Auto g)
=> arr (\c -> fst (g c)) (now let f = Auto g)
=> arr (fst . g)
=> fix $ \auto -> Auto (\i -> ((fst . g) i, auto))

RHS:
f
=> Auto g (f = Auto g)
=> Auto (\i -> let v = g i in (fst v, snd v))

now that on LHS we have "auto" whereas "snd (g i)" on RHS,
and it all depends on "g" (or "f") to determine whether LHS and RHS are equal.

-}

{-

the following example is just for showing that extensionality axiom fails
and should never be used anywhere else:

-}

instance ArrowApply Auto where
    app = arr (\(Auto f, x) -> fst (f x))

testF :: Auto Int Int
testF = Auto (\i -> (succ i, fix $ \auto -> Auto (\i' -> (i',auto))))

test1 :: Auto Int Int
test1 = mkPair testF >>> app

test2 :: Auto Int Int
test2 = testF

{-

this is a clear example that extensionality fails:

> runAuto test1 [0..4]
[1,2,3,4,5]
> runAuto test2 [0..4]
[1,1,2,3,4]

-}
