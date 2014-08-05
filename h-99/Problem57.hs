module Problem57
where

import BinaryTree
import Problem56 (symmetric)

construct :: Ord a => [a] -> Tree a
construct = foldr insert Empty . reverse

insert :: Ord a => a -> Tree a -> Tree a
insert v Empty = singleton v
insert v (Branch v1 l r) =
    if v <= v1
       then Branch v1 (insert v l) r
       else Branch v1 l (insert v r)

main :: IO ()
main = do
    print $ construct [3 :: Int, 2, 5, 7, 1]
    print $ symmetric . construct $ [5 :: Int, 3, 18, 1, 4, 12, 21]
    print $ symmetric . construct $ [3 :: Int, 2, 5, 7, 1]
    print $ symmetric . construct $ [3 :: Int, 2, 5, 7, 4]
