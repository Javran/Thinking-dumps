module Ch05Exercise4 where

import Ch05Splay

-- just the naive version
smaller :: Ord a => a -> Tree a -> Tree a
smaller _ E = E
smaller pivot (T a x b) =
    if x > pivot
       then smaller pivot a
       else T a x (smaller pivot b)

smaller' :: Ord a => a -> Tree a -> Tree a
smaller' pivot (T a x b) =
    if x > pivot
       then smaller' pivot a
       else case a of
              E -> T a x E
              T a1 y a2 ->
                  smaller pivot (T a x b)
