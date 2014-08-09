module Problem62B where

import BinaryTree

atLevel :: Tree a -> Int -> [a]
atLevel _ n | n <= 0 = []
atLevel Empty _ = []
atLevel (Branch v _ _) 1 = [v]
atLevel (Branch _ l r) n = atLevel l (n-1) ++ atLevel r (n-1)

main :: IO ()
main = print $ atLevel tree4 2
