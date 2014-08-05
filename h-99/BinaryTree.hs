module BinaryTree where

data Tree a
    = Empty
    | Branch a (Tree a) (Tree a)
      deriving (Show, Eq)

singleton, leaf :: a -> Tree a
singleton v = Branch v Empty Empty
leaf = singleton
