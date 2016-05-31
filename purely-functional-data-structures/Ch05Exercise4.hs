module Ch05Exercise4 where

import Ch05Splay

-- just the naive version
smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller pivot (T a x b) =
    if x > pivot
       then smaller pivot a
       else T a x (smaller pivot b)
