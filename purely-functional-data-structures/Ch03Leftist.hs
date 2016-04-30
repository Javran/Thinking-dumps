module Ch03Leftist where

data Heap a = E | T Int a (Heap a) (Heap a)

empty :: Heap a
empty = E

isEmpty :: Heap a -> Bool
isEmpty E = True
isEmpty _ = False
