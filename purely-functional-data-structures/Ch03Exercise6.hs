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
