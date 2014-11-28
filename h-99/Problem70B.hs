module Problem70B where

import MTree

-- no need to check for the tree,
-- we get this exercise for free
main :: IO ()
main = mapM_ print [tree1,tree2,tree3,tree4,tree5]
