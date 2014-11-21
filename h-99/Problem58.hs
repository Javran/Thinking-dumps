module Problem58
where

import BinaryTree
import Problem56 (symmetric)

import Control.Applicative

genTrees :: Int -> [Tree Char]
genTrees 0 = [Empty]
genTrees 1 = [singleton 'x']
genTrees n = do
    l <- [0..n-1]
    let r = n - 1 - l
    Branch 'x' <$> genTrees l <*> genTrees r

height :: Tree a -> Int
height Empty = 0
height (Branch _ l r) = 1 + max (height l) (height r)

isBalanced :: Tree a -> Bool
isBalanced Empty = True
isBalanced (Branch _ l r) =
       isBalanced l
    && isBalanced r
    && abs (height l - height r) <= 1

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter ((&&) <$> isBalanced <*> symmetric) . genTrees

main :: IO ()
main = mapM_ print $ symCbalTrees 5
