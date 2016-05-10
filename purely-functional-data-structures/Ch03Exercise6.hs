module Ch03Exercise6 where

data Tree a = Node a [Tree a]

type Heap a = [(Int, Tree a)]

rank :: Tree a -> Int
rank (Node _ ts) = length ts

root :: Tree a -> a
root (Node x _) = x

singleton :: a -> Tree a
singleton v = Node v []

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null

-- | "link" should only be used on trees of the same rank
link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node x1 c1) t2@(Node x2 c2) =
    if x1 <= x2
      then Node x1 (t2 : c1)
      else Node x2 (t1 : c2)

insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [(rank t, t)]
insTree t ts@((_,t'):ts') =
    if rank t < rank t'
      then (rank t, t) : ts
      else insTree (link t t') ts'

{-# ANN insert "HLint: ignore Eta reduce" #-}
-- creates a singleton tree, carries it until an unoccupied rank
-- is found in the list of trees
insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree (singleton x) ts

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@((_,t1):ts1') ts2@((_,t2):ts2')
    | rank t1 < rank t2 = (rank t1, t1) : merge ts1' ts2
    | rank t1 > rank t2 = (rank t2, t2) : merge ts1 ts2'
    | otherwise =
        -- on this branch we know rank r = rank t1 == rank t2
        -- and that r+1 <= head ts1' and r+1 <= head ts2'
        -- so the function call above is safe.
        insTree (link t1 t2) (merge ts1' ts2')
