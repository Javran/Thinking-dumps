{-# LANGUAGE MonadComprehensions #-}
module CollatzConjecture (collatz) where

import Data.List

collatz :: Integer -> Maybe Integer
collatz i = [genericLength . takeWhile (/= 1) . iterate next $ i | i > 0]
  where
    next x = if even x then x `quot` 2 else x * 3 + 1
