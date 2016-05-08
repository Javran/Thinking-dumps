module Ch03Binominal where

data Tree a = Node Int a [Tree a]

type Heap a = [Tree a]

-- | "link" should only be used on trees of the same rank
link :: Ord a => Tree a -> Tree a -> Tree a
link t1@(Node r x1 c1) t2@(Node _ x2 c2) =
    if x1 <= x2
      then Node (r+1) x1 (t2 : c1)
      else Node (r+1) x2 (t1 : c2)

rank :: Tree a -> Int
rank (Node v _ _) = v

-- | "insTree" is an internal function,
-- | "insTree t ts" should only be called when rank t <= rank t'
-- | otherwise trees of different ranks will be passed to "link"
-- | breaking its invariant.
insTree :: Ord a => Tree a -> Heap a -> Heap a
insTree t [] = [t]
insTree t ts@(t':ts') =
    if rank t < rank t'
      then t : ts
      else insTree (link t t') ts'

singleton :: a -> Tree a
singleton v = Node 0 v []

{-# ANN insert "HLint: ignore Eta reduce" #-}
-- creates a singleton tree, carries it until an unoccupied rank
-- is found in the list of trees
insert :: Ord a => a -> Heap a -> Heap a
insert x ts = insTree (singleton x) ts
