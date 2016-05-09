module Ch03Binominal
  ( Heap
  , root
  , rank
  , empty
  , singleton
  , isEmpty
  , merge

  , findMin
  , deleteMin

  , insert

  ) where

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
-- | "insTree t ts" should only be called when "ts" is empty or "rank t <= rank (head ts)"
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

merge :: Ord a => Heap a -> Heap a -> Heap a
merge ts1 [] = ts1
merge [] ts2 = ts2
merge ts1@(t1:ts1') ts2@(t2:ts2')
    | rank t1 < rank t2 = t1 : merge ts1' ts2
    | rank t1 > rank t2 = t2 : merge ts1 ts2'
    | otherwise =
        -- on this branch we know rank r = rank t1 == rank t2
        -- and that r+1 <= head ts1' and r+1 <= head ts2'
        -- so the function call above is safe.
        insTree (link t1 t2) (merge ts1' ts2')

root :: Tree a -> a
root (Node _ v _) = v

viewMinTree :: Ord a => Heap a -> Maybe (Tree a, Heap a)
viewMinTree [x] = Just (x,[])
viewMinTree (t:ts) = do
    (t',ts') <- viewMinTree ts
    Just $ if root t <= root t'
      then (t,ts)
      else (t',t:ts')
viewMinTree [] = Nothing

findMin :: Ord a => Heap a -> Maybe (Tree a)
findMin x =  fst <$> viewMinTree x

deleteMin :: Ord a => Heap a -> Maybe (Heap a)
deleteMin x = snd <$> viewMinTree x

empty :: Heap a
empty = []

isEmpty :: Heap a -> Bool
isEmpty = null
