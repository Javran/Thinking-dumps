module Ch03Leftist where

data Heap a = E | T Int a (Heap a) (Heap a)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False

merge :: Ord a => Heap a -> Heap a -> Heap a
merge l E = l
merge E r = r
merge h1@(T _ x a1 b1) h2@(T _ y a2 b2) =
  if x <= y
    then makeT x a1 (merge b1 h2)
    else makeT y a2 (merge h1 b2)

rank :: Heap a -> Int
rank E = 0
rank (T rnk _ _ _) = rnk

makeT :: Ord a => a -> Heap a -> Heap a -> Heap a
makeT x a b = if rank a >= rank b
  then T (rank b+1) x a b
  else T (rank a+1) x b a
