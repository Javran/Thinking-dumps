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
smaller' _ E = E
smaller' pivot (T a x b) =
    if x > pivot
       then smaller' pivot a
       else case b of
          E -> T a x E
          T b1 y b2 ->
              if y > pivot
                 then T a x (smaller' pivot b1)
                 else T (T a x b1) y (smaller' pivot b2)
