module Ch05Splay where

data Tree a
  = E
  | T (Tree a) a (Tree a)

bigger :: Ord a => a -> Tree a -> Tree a
bigger _ E = E
bigger pivot (T a x b) =
    if x <= pivot
       then bigger pivot b
       else case a of
              E -> T E x b
              T a1 y a2 ->
                  if y <= pivot
                     then T (bigger pivot a2) x b
                     else T (bigger pivot a1) y (T a2 x b)
