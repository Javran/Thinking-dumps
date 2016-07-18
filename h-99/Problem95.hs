module Problem95 where

import qualified Data.Array as Arr

digitToWordArr :: Arr.Array Int String
digitToWordArr = Arr.array (0,9) (zip [0..9] digitWords)
  where
    digitWords = words
        "zero one two three four \
        \five six seven eight nine"

digitToWord :: Int -> String
digitToWord = (digitToWordArr Arr.!)

breakDigits :: Integer -> [Int]
breakDigits 0 = [0]
breakDigits v = brkDigits [] v
  where
    brkDigits acc 0 = acc
    brkDigits acc x = brkDigits (r':acc) q
        where
          (q,r) = x `quotRem` 10
          r' = fromInteger r
