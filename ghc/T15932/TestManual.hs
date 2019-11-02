module Cov where
newtype F a = F a
instance Functor F where
  fmap f (F x) = F (f x)
