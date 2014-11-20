module BinaryTree where

import Data.Monoid
import Data.Foldable
import Data.Function

data Tree a
    = Empty
    | Branch a (Tree a) (Tree a)
      deriving (Show, Eq)

-- | preorder traversal for trees
instance Foldable Tree where
    foldMap _ Empty = mempty
    foldMap f (Branch v l r) = f v <> foldMap f l <> foldMap f r

-- | tree depth is defined as
--   the length of the longest path from root to any leaves
depth :: Tree a -> Int
depth Empty = 0
depth (Branch _ l r) = succ $ (max `on` depth) l r

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
