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

genSym1 :: Enum e => State e () e
genSym1 = ((returnA >>> fetch) &&& returnA)
    >>> ((arr (\ (n,_) -> succ n) >>> store) &&& returnA)
    >>> arr (\(_,(n,_)) -> n)
