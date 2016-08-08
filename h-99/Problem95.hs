module Problem95
  ( breakDigits
  , fullWords
  ) where

import qualified Data.Array as Arr
import Data.List

{-
  (internal only) converting digits to words,
  note that the input should only be [0..9]
-}
digitToWord :: Int -> String
digitToWord = (digitToWordArr Arr.!)
  where
    {-
      despite that the following definitions are locally scoped
      to this function, it should be easy for GHC to recognize them
      as CAFs and lift them out. doing a profiling with "-caf-all"
      should be able to confirm this.
    -}
    digitToWordArr = Arr.array (0,9) (zip [0..9] digitWords)
    digitWords = words
        "zero one two three four \
        \five six seven eight nine"

-- | breaks non-negative integer into an non-empty list of digits.
--   it is guaranteed that the resulting list is non-empty, and every
--   element of it is within range [0..9]
breakDigits :: Integer -> [Int]
breakDigits 0 = [0]
breakDigits v = brkDigits [] v
  where
    brkDigits acc 0 = acc
    brkDigits acc x = brkDigits (r':acc) q
        where
          (q,r) = x `quotRem` 10
          r' = fromInteger r

fullWords :: Integer -> String
fullWords =
      intercalate "-"
    . map digitToWord
    . breakDigits
