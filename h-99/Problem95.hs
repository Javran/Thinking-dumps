module Problem95
  ( breakDigits
  , fullWords
  ) where

import qualified Data.Array as Arr
import Data.List

{-
  (internal only) an array of digit-to-word mapping.
  TODO: I'm making this value top-level in hope that
  we don't recalculate it every time we demands it.
  but I think this is an easy-to-spot chance of optimization,
  and doubt we should do this manually.
-}
digitToWordArr :: Arr.Array Int String
digitToWordArr = Arr.array (0,9) (zip [0..9] digitWords)
  where
    digitWords = words
        "zero one two three four \
        \five six seven eight nine"

{-
  (internal only) converting digits to words,
  note that the input should only be [0..9]
-}
digitToWord :: Int -> String
digitToWord = (digitToWordArr Arr.!)

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
