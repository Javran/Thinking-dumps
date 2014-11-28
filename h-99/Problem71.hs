module Problem71 where

import MTree
import Prelude hiding (sum)
import Data.Foldable
import Control.Applicative

-- | interal path length
--   except for the root tree, each subtree in this tree
--   has exactly one edge which links this subtree to its parent node
--   and this edge will be counted as many times as the number
--   of nodes that this subtree has
--   therefore we first calculate the size of the subtree for each node
--   take the sum of the whole tree, but remove the size of the whole tree
--   because root node does not have a parent.
ipl :: Tree a -> Int
ipl = ((-) <$> sum <*> rootLabel) . countTree

-- | replace root label of each node
--   with the number of nodes of this subtree
countTree :: Tree a -> Tree Int
countTree (Node _ sub) = Node (1+sum (map sum subs)) subs
  where
    subs = map countTree sub

main :: IO ()
main = do
    print (ipl tree5)
    print (ipl tree4)

