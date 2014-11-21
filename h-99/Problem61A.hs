module Problem62
where

import Data.Function
import BinaryTree

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch v Empty Empty) = [v]
leaves (Branch _ l r) = ((++) `on` leaves) l r

main :: IO ()
main = print . leaves $ tree4
