module Ch03RedBlack where

data Color = Rd | Bk

data Tree a = E | T Color (Tree a) a (Tree a)

empty :: Tree a
empty = E

member :: (Ord a) => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True
