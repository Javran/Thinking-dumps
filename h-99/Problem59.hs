module Problem59 where

import Control.Applicative

import BinaryTree

-- generate trees whose height is equal to a given number
genTreesHeightEq :: Char -> Int -> [Tree Char]
genTreesHeightEq _ 0 = [Empty]
genTreesHeightEq c 1 = [singleton c]
genTreesHeightEq c n = do
    let generators = [genTreesHeightLt c, genTreesHeightEq c]
    -- we drop the first generator pair because we need to have at least one child
    -- whose height is (n-1)
    (genL,genR) <- tail $ (,) <$> generators <*> generators
    Branch 'x' <$> genL (n-1) <*> genR (n-1)

-- generate trees whose height is less than a given number
genTreesHeightLt :: Char -> Int -> [Tree Char]
genTreesHeightLt c n = concatMap (genTreesHeightEq c) [0..n-1]

hbalTree :: Char -> Int -> [Tree Char]
hbalTree = genTreesHeightEq

main :: IO ()
main = do
    mapM_ print $ hbalTree 'x' 3
    -- see: https://oeis.org/A001699
    print $ map (length . genTreesHeightEq 'x') [0..5]
