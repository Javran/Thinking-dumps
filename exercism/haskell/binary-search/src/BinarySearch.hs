module BinarySearch (find) where

import Control.Monad
import qualified Data.Array as Arr

find :: Ord a => Arr.Array Int a -> a -> Maybe Int
find arr x = search lo hi
  where
    (lo, hi) = Arr.bounds arr
    search l r = do
      guard $ l <= r
      let mid = (l + r) `quot` 2
      case compare x (arr Arr.! mid) of
        LT -> search l (mid -1)
        EQ -> Just mid
        GT -> search (mid + 1) r
