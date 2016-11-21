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
    (CPSF g) . (CPSF f) = CPSF (g >>> f)

instance ArrowApply a => Arrow (CPSFunctor r a) where
    arr f = CPSF (\k -> arr f >>> k)
    first (CPSF f) = CPSF
        (\k -> arr (\(b,d) -> (f (arr (\c->(c,d)) >>> k),b)) >>> app)
