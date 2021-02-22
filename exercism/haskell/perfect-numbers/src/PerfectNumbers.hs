{-# LANGUAGE PatternSynonyms #-}

module PerfectNumbers
  ( classify
  , Classification
  , pattern Deficient
  , pattern Perfect
  , pattern Abundant
  )
where

import Control.Monad
import Data.Coerce

newtype Classification = Cls Ordering deriving (Show, Eq)

-- bidirectional patterns observing the isomorphism.
pattern Deficient = Cls LT

pattern Perfect = Cls EQ

pattern Abundant = Cls GT

classify :: Int -> Maybe Classification
classify x = do
  guard (x > 0)
  Just . coerce $ compare (sum aliquot) x
  where
    aliquot = [y | y <- [1 .. x -1], x `rem` y == 0]
