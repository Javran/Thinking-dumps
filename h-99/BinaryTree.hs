module BinaryTree where

data Tree a
    = Empty
    | Branch a (Tree a) (Tree a)
      deriving (Show, Eq)

singleton, leaf :: a -> Tree a
singleton v = Branch v Empty Empty
leaf = singleton

tree1 :: Tree Char
tree1 = Branch 'a' (Branch 'b' (leaf 'd')
                               (leaf 'e'))
                   (Branch 'c' Empty
                               (Branch 'f' (leaf 'g')
                                           Empty))

tree2 :: Tree Char
tree2 = Branch 'a' Empty Empty

tree3 :: Tree Char
tree3 = Empty

tree4 :: Tree Int
tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
