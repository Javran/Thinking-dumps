{-# LANGUAGE DeriveFunctor #-}
module Cov2 where
newtype F a = F a deriving Functor
newtype G a = G a
instance Functor G where
  fmap f (G x) = G (f x)
