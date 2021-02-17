module Hamming (distance) where

import Control.Monad
import Data.Monoid
import Util (cmpList)

-- | calculate the hamming distance between two strings
distance :: String -> String -> Maybe Int
distance xs ys = do
  guard $ cmpList (\_ _ -> EQ) xs ys == EQ
  pure . getSum . mconcat $ zipWith (\x y -> if x == y then 0 else 1) xs ys
