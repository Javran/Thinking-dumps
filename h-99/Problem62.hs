module Problem62
where

import Data.Function
import BinaryTree

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch v l r) = v : ((++) `on` internals) l r

main :: IO ()
main = print . internals $ tree4
