module Ch03Exercise4 where

-- the difference between leftist heap and
-- weight-biased leftist heap is not too large

data Heap a = E | T Int a (Heap a) (Heap a)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

rank :: Heap a -> Int
rank E = 0
rank (T rnk _ _ _) = rnk

singleton :: a -> Heap a
singleton x = T 1 x E E

makeT :: Ord a => a -> Heap a -> Heap a -> Heap a
makeT x a b = if rank a >= rank b
    then T newRank x a b
    else T newRank x b a
  where
    -- unlike leftist heap,
    -- in weighted biased version
    -- the rank is the size of the tree
    newRank = rank a + rank b + 1

-- seems we can keep "merge" the same
-- as it has nothing to do with the rank we are modifying
merge :: Ord a => Heap a -> Heap a -> Heap a
merge l E = l
merge E r = r
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
  if x <= y
    then makeT x a1 (merge b1 h2)
    else makeT y a2 (merge h1 b2)
