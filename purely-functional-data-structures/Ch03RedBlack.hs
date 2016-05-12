module Ch03RedBlack where

data Color = Red | Black

data Tree a = E | T Color (Tree a) a (Tree a)

empty :: Tree a
empty = E

member :: (Ord a) => a -> Tree a -> Bool
member _ E = False
member x (T _ a y b)
    | x < y = member x a
    | x > y = member x b
    | otherwise = True

balance :: Color -> Tree a -> a -> Tree a -> Tree a
balance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d)
balance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red (T Red b y c) z d) = T Red (T Black a x b) y (T Black c z d)
balance Black a x (T Red b y (T Red c z d)) = T Red (T Black a x b) y (T Black c z d)
balance color l v r = T color l v r
