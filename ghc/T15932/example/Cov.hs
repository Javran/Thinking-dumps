{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Cov where

import Control.DeepSeq

newtype F a =
    F (Int, a)
  deriving (Functor, NFData)


newtype G a =
    G (Int, a)
  deriving (NFData)

instance Functor G where
    fmap f (G (a, b)) = G (a, f b)
