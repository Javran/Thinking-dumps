module Problem61
where

import Data.Function
import BinaryTree

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = ((+) `on` countLeaves) l r

main :: IO ()
main = print . countLeaves $ tree4
