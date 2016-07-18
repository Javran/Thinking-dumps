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
