module MTree
  ( module MTree
  , module Data.Tree
  ) where

-- MTree for MultiWayTree

import Data.Tree

tree1, tree2, tree3, tree4, tree5 :: Tree Char

tree1 = Node 'a' []
tree2 = Node 'a' [Node 'b' []]
tree3 = Node 'a' [Node 'b' [Node 'c' []]]
tree4 = Node 'b' [Node 'd' [], Node 'e' []]
tree5 = Node 'a' [Node 'f' [Node 'g' []]
                 ,Node 'c' []
                 , Node 'b' [Node 'd' [], Node 'e' []]
                 ]
