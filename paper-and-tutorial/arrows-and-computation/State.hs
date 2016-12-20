{-# LANGUAGE InstanceSigs, ScopedTypeVariables, Arrows #-}
module State where

import qualified Control.Category as Cat
import Control.Arrow
import Common

newtype State s i o = ST { runST :: (s,i) -> (s,o) }

{-# ANN arrS "HLint: ignore Use second" #-}
arrS :: (i -> o) -> State s i o
arrS f = ST $ \(s,i) -> (s,f i)

compS :: State s a b -> State s b c -> State s a c
compS (ST f) (ST g) = ST (g . f)

firstS :: State s a b -> State s (a,d) (b,d)
firstS (ST f) = ST $ \(s,(a,d)) -> let (s',b) = f (s,a) in (s',(b,d))

instance Cat.Category (State s) where
    id = arrS id
    g . f = compS f g

instance Arrow (State s) where
    arr = arrS
    first = firstS

instance ArrowChoice (State s) where
    left (ST f) = ST $ \ (~(s,e)) -> case e of
        Left b
            | (s',c) <- f (s,b)
            -> (s', Left c)
        Right d -> (s, Right d)

instance ArrowLoop (State s) where
    -- note that most of the following type signatures are
    -- not necessary, but I think it helps to write them out
    loop :: forall b c d. State s (b,d) (c,d) -> State s b c
    loop (ST (f :: (s,(b,d)) -> (s,(c,d)))) =
        -- (first impl)
        -- ST $ \(s,b) -> let (s',(c,d)) = f (s,(b,d)) in (s',c)
        -- (alternative)
        -- ST $ \sb -> let (sc,d) = f' (sb,d) in sc
        -- (alternative)
        ST $ trace f'
      where
        f' :: ((s,b),d) -> ((s,c),d)
        f' = unassoc . f . assoc

fetch :: State s () s
fetch = ST $ \ (~(s,_)) -> (s,s)

store :: State s s ()
store = ST $ \ (_,s') -> (s',())

-- genSym takes a unit as input and produce an enumerable value (whatever
-- that can perform "succ" on it)
genSym :: Enum e => State e () e
genSym = proc inp -> do
    -- note that "inp" is just "()"
    -- because "()" is a singeton type (without considering non-termination)
    n <- fetch -< inp
    -- we fetch the value, bump it and keep the value we initially get
    -- as return value.
    _ <- store -< succ n
    returnA -< n

{-

first version without comment:

genSym1 :: Enum e => State e () e
genSym1 = proc inp -> do
    n <- fetch -< inp
    _ <- store -< succ n
    returnA -< n

adding brackets and semicolon:

genSym1 :: Enum e => State e () e
genSym1 = proc inp -> do {
    n <- fetch -< inp;
    _ <- store -< succ n;
    returnA -< n;
    }

apply translation rule, notice the first command is of the form "p' <- c; B":

genSym1 :: Enum e => State e () e
genSym1 =
    ((proc inp -> fetch -< inp) &&& returnA) >>>
    proc (n,inp) -> do {
        _ <- store -< succ n;
        returnA -< n;
    }

now apply translation rule on "proc inp -> fetch -< inp"

genSym1 :: Enum e => State e () e
genSym1 =
    ((arr (\inp -> inp) >>> fetch) &&& returnA) >>>
    proc (n,inp) -> do {
        _ <- store -< succ n;
        returnA -< n;
    }

now focus on the remaining "proc" part, whose first command is again of the form "p' <- c; B"

genSym1 :: Enum e => State e () e
genSym1 =
    ((arr (\inp -> inp) >>> fetch) &&& returnA) >>>
    ((proc (n,inp) -> store -< succ n) &&& returnA) >>>
    proc (_,(n,inp)) -> do { returnA -< n }

now translate first "proc" part:

genSym1 :: Enum e => State e () e
genSym1 =
    ((arr (\inp -> inp) >>> fetch) &&& returnA) >>>
    ((arr (\(n,inp) -> succ n) >>> store) &&& returnA) >>>
    proc (_,(n,inp)) -> do { returnA -< n }

and then the last "proc" part:

genSym1 :: Enum e => State e () e
genSym1 =
    ((arr (\inp -> inp) >>> fetch) &&& returnA) >>>
    ((arr (\(n,inp) -> succ n) >>> store) &&& returnA) >>>
    arr (\(_,(n,inp)) -> n) >>> returnA

at this point ghc and HLint has been giving many suggestions,
most of which should be obvious:

genSym1 :: Enum e => State e () e
genSym1 =
    ((arr id >>> fetch) &&& returnA) >>>
    ((arr (\(n,()) -> succ n) >>> store) &&& returnA) >>>
    arr (\(_,(n,())) -> n) >>> returnA

notice that by using type holes, I managed to get type of both "inp" to be "()",
which should be a clear hint that this arrow network can be more simple.

now we can eliminate some "returnA", also notice "arr id" is just the definition of "returnA"
so it can also be removed.

genSym1 :: Enum e => State e () e
genSym1 =
    (fetch &&& returnA) >>>
    ((arr (\(n,()) -> succ n) >>> store) &&& returnA) >>>
    arr (\(_,(n,())) -> n)

here we can find a pattern "f &&& returnA" occurs in 2 places, let figure out what it does:

f &&& returnA
=> arr (\x -> (x,x)) >>> f *** returnA (definition)
=> arr (\x -> (x,x)) >>> first f >>> arr swap >>> second returnA >>> arr swap (definition)
=> arr (\x -> (x,x)) >>> first f >>> arr swap >>> arr swap (property of returnA)
=> arr (\x -> (x,x)) >>> first f >>> returnA (swap . swap === id)
=> arr (\x -> (x,x)) >>> first f

let's use this result on genSym1:

genSym1 :: Enum e => State e () e
genSym1 =
    (arr (\x -> (x,x)) >>> first fetch) >>>
    (arr (\x -> (x,x)) >>> first (arr (\(n,()) -> succ n) >>> store)) >>>
    arr (\(_,(n,())) -> n)

now use the same trick before to see if we can get more "()"s in the result:

genSym1 :: Enum e => State e () e
genSym1 =
    (arr (\() -> ((),())) >>> first fetch) >>>
    (arr (\(x,()) -> ((x,()),(x,()))) >>> first (arr (\(n,()) -> succ n) >>> store)) >>>
    arr (\(_,(n,())) -> n)

"arr (\() -> ((),())) >>> first fetch" is clearly just "fetch >>> arr (\n -> (n,()))":

genSym1 :: Enum e => State e () e
genSym1 =
    fetch >>> arr (\n -> (n,())) >>>
    (arr (\(x,()) -> ((x,()),(x,()))) >>> first (arr (\(n,()) -> succ n) >>> store)) >>>
    arr (\(_,(n,())) -> n)

also notice:

first (arr (\(n,()) -> succ n) >>> store)
=> first (arr (\(n,()) -> succ n)) >>> first store (property)
=> arr (first (\(n,()) -> succ n)) >>> first store (property)

we then have (removing unnecessary parentheses):

genSym1 :: Enum e => State e () e
genSym1 =
    fetch >>> arr (\n -> (n,())) >>>
    arr (\(x,()) -> ((x,()),(x,()))) >>> arr (first (\(n,()) -> succ n)) >>>
    first store >>>
    arr (\(_,(n,())) -> n)

we can remove a unnecessary passing of "()":

arr (\(x,()) -> ((x,()),(x,()))) >>> arr (first (\(n,()) -> succ n))
=> arr (\(x,()) -> (x,(x,()))) >>> arr (first (\n -> succ n))
=> arr (\(x,()) -> (x,(x,()))) >>> arr (first succ)
=> arr ((\(x,()) -> (x,(x,()))) >>> first succ) (property)
=> arr (\(x,()) -> (succ x,(x,())))

now we end up with:

genSym1 :: Enum e => State e () e
genSym1 =
    fetch >>> arr (\n -> (n,())) >>>
    arr (\(x,()) -> (succ x,(x,()))) >>>
    first store >>>
    arr (\(_,(n,())) -> n)

now, clearly we are passing an "()" throughout this network for no particular reason,
so we can get rid of it:

genSym1 :: Enum e => State e () e
genSym1 =
    fetch >>> arr (\n -> (n {- ,() -})) >>>
    arr (\(x {- ,() -}) -> (succ x,(x{- ,() -}))) >>>
    first store >>>
    arr (\(_,(n{- ,() -})) -> n)

this indeed works, so we can further simplify:

genSym1 :: Enum e => State e () e
genSym1 =
    fetch >>> arr id >>>
    arr (\x -> (succ x,x)) >>>
    first store >>>
    arr snd

so we now have the final version:
-}
genSym1 :: Enum e => State e () e
genSym1 =
    fetch >>> -- fetch value from the register
    arr (\x -> (succ x,x)) >>> -- bump counter, keep original value.
    first store >>> -- store new value
    arr snd -- get the fetched value as result
