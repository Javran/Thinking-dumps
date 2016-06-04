module Ch05Exercise4 where

import Ch05Splay

-- the naive version
smaller1 :: Ord a => a -> Tree a -> Tree a
smaller1 _ E = E
smaller1 pivot (T a x b) =
    if x > pivot
       then smaller1 pivot a
       else T a x (smaller1 pivot b)

-- with node rotation
smaller2 :: Ord a => a -> Tree a -> Tree a
smaller2 _ E = E
smaller2 pivot (T a x b) =
    if x > pivot
       then smaller2 pivot a
       else case b of
          E -> T a x E
          T b1 y b2 ->
              if y > pivot
                 then T a x (smaller2 pivot b1)
                 else T (T a x b1) y (smaller2 pivot b2)
