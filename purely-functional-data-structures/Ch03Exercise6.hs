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
