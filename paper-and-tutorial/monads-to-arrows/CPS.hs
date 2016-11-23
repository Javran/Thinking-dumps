module CPS where

import Control.Arrow
import qualified Control.Category as Cat

-- usually it's just (a -> r) -> r,
-- but we can still recover it by m ~ Identity
newtype CPS r m a = CPS ((a -> m r) -> m r)

newtype CPSFunctor r a b c = CPSF (a c r -> a b r)

liftCPS :: Arrow a => a b c -> CPSFunctor r a b c
liftCPS f = CPSF (f >>>)

instance Cat.Category (CPSFunctor r a) where
    id = CPSF id
    -- just to show that "CPSF" composes in the reversed order
    -- we are guided by the type, so actually there's less surprise
    (CPSF g) . (CPSF f) = CPSF (f . g)

instance ArrowApply a => Arrow (CPSFunctor r a) where
    arr f = CPSF (\k -> arr f >>> k)
    first (CPSF f) = CPSF
        (\k -> arr (\(b,d) -> (f (arr (\c->(c,d)) >>> k),b)) >>> app)

-- TODO: I'm wondering if we can try to make examples for "jump" and "callcc"
-- TODO: also figure out dynamic choice and application

{-# ANN jump "HLint: ignore Use const" #-}
jump :: ArrowApply a => CPSFunctor ans a (a c ans, c) z
jump = CPSF (\_ -> app)

-- GHC says "ArrowApply a" is a redundant constraint.
callcc :: (a c ans -> CPSFunctor ans a b c) -> CPSFunctor ans a b c
callcc f = CPSF (\k -> let CPSF g = f k in g k)
