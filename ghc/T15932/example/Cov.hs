{-# LANGUAGE DeriveFunctor #-}
{-# OPTIONS_GHC -ddump-to-file -ddump-ds -fhpc #-}
module Cov where

-- F has Functor instance using deriving mechanism
newtype F a = F (Int, a) deriving (Functor)

-- G has Functor but is manually defined.
newtype G a = G (Int, a)

instance Functor G where
    fmap f (G (a, b)) = G (a, f b)
