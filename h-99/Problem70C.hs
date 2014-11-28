module Problem70C where

import MTree
import Prelude hiding (sum)
import Data.Foldable

nnodes :: Tree a -> Int
nnodes = length . toList

main :: IO ()
main = print (nnodes tree2)
