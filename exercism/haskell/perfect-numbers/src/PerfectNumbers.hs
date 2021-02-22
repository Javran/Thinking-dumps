module PerfectNumbers
  ( classify
  , Classification (..)
  )
where

import Control.Monad
import Unsafe.Coerce (unsafeCoerce)

data Classification
  = Deficient
  | Perfect
  | Abundant
  deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify x = do
  guard (x > 0)
  Just . unsafeCoerce $ compare (sum aliquot) x
  where
    aliquot = [y | y <- [1 .. x -1], x `rem` y == 0]
