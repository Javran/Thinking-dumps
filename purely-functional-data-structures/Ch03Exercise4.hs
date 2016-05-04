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
