module CollatzConjecture (collatz) where

import Control.Monad
import Data.List

collatz :: Integer -> Maybe Integer
collatz i = do
  guard $ i > 0
  pure . genericLength . takeWhile (/= 1) . iterate next $ i
  where
    next x = if even x then x `quot` 2 else x * 3 + 1
