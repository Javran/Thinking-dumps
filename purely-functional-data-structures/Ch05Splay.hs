module Ch05Splay where

import Data.Foldable

-- Splay tree's properties are very similar to that of binary search tree's:
-- + every non-leave node contains an element
-- + all left subtree nodes are less than (or equal to) that of root's
-- + all right subtree nodes are greater than (or equal to) that of root's
-- (note: usually the equality is expressed as "if a <= b then something_1 else something_2",
--  that's where the "or equal to" part comes from)
-- (note: one difference between splay tree and binary search tree is that the former
--  allows duplicated elements)
data Tree a
  = E
  | T (Tree a) a (Tree a)
    deriving (Show)

empty :: Tree a
empty = E

isEmpty :: Tree a -> Bool
isEmpty E = True
isEmpty _ = False

-- In book the purpose of this operation is to "extract a bigger subtree",
-- but I feel this is confusing. What this operation really does is
-- to rebuild a subtree by collecting all elements that are bigger than the "pivot" node
-- and "extracting" gives me the impression of returning just one node from the tree.
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
                     else
                       -- rotate nodes whenever we have followed two left branches
                       -- this operation tends to result in a more balanced tree.
                       T (bigger pivot a1) y (T a2 x b)

findMin :: Tree a -> Maybe a
findMin = (fst <$>) . viewMin

deleteMin :: Tree a -> Maybe (Tree a)
deleteMin = (snd <$>) . viewMin

viewMin :: Tree a -> Maybe (a, Tree a)
viewMin E = Nothing
viewMin (T E x b) = Just (x, b)
viewMin (T (T E x b) y c) = Just (x, T b y c)
viewMin (T (T a x b) y c) = do
    (a',rest) <- viewMin a
    pure (a', T rest x (T b y c))

partition :: Ord a => a -> Tree a -> (Tree a, Tree a)
partition _ E = (E, E)
partition pivot t@(T a x b) =
    if x <= pivot
      then
        case b of
          E -> (t,E)
          T b1 y b2 ->
            if y <= pivot
              then
                let (small,big) = partition pivot b2
                in (T (T a x b1) y small, big)
              else
                let (small,big) = partition pivot b1
                in (T a x small, T big y b2)
      else
        -- x > pivot
        case a of
          E -> (E,t)
          T a1 y a2 ->
            if y <= pivot
              then
                let (small,big) = partition pivot a2
                in (T a1 y small, T big x b)
              else
                let (small,big) = partition pivot a1
                in (small, T big y (T a2 x b))

insert :: Ord a => a -> Tree a -> Tree a
insert x t = T a x b
  where
    (a,b) = partition x t

merge :: Ord a => Tree a -> Tree a -> Tree a
merge E t = t
merge (T a x b) t = T (merge ta a) x (merge tb b)
  where
    (ta,tb) = partition x t

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip insert) empty

-- converting a splay tree into a list
toAscList :: Tree a -> [a]
toAscList t = case viewMin t of
    Nothing -> []
    Just (v,t') -> v : toAscList t'
