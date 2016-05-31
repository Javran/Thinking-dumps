module Ch05Splay where

-- Splay tree's properties are very similar to that of binary search tree's:
-- + every non-leave node contains an element
-- + all left subtree nodes are less than (or equal to) that of root's
-- + all right subtree nodes are greater than (or equal to) that of root's
-- (note: usually the equality is expressed as "if a <= b then something_1 else something_2",
--  that's where the "or equal to" part comes from)
data Tree a
  = E
  | T (Tree a) a (Tree a)

-- here I'm unclear about what "extract a bigger subtree" means
-- need some explanation here to understand.

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
