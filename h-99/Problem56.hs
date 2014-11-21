module Problem56 where

import BinaryTree

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 r1) (Branch _ l2 r2) =
    mirror l1 r2 && mirror r1 l2
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

main :: IO ()
main = print (symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty))
    >> print (symmetric (Branch 'x' (Branch 'x' Empty Empty) (Branch 'x' Empty Empty)))
