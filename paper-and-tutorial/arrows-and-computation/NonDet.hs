{-# LANGUAGE TupleSections #-}
module NonDet where

import qualified Control.Category as Cat
import Control.Arrow
import Control.Monad

-- the Kleisli arrow of list monad in disguise
newtype NonDet i o = ND { runND :: i -> [o] }

arrS :: (i -> o) -> NonDet i o
arrS f = ND (pure . f)

compS :: NonDet a b -> NonDet b c -> NonDet a c
compS (ND f) (ND g) = ND (f >=> g)

firstS :: NonDet a b -> NonDet (a,d) (b,d)
firstS (ND f) = ND $ \(a,d) -> (,d) <$> f a

instance Cat.Category NonDet where
    id = arrS id
    g . f = compS f g

instance Arrow NonDet where
    arr = arrS
    first = firstS

instance ArrowChoice NonDet where
    left (ND f) = ND $ \e -> case e of
        Left b -> Left <$> f b
        Right d -> pure (Right d)
