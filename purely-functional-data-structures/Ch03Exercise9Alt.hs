module Ch03Exercise9Alt where

import Ch03RedBlack
import Control.DeepSeq

buildTree :: Int -> (Int, Tree ())
buildTree 0 = (0, E)
buildTree 1 = (0, T Red E () E)
buildTree 2 = (1, T Black (T Red E () E) () E)
buildTree n = (if rootColor == Red then rH else rH+1, T rootColor lTree () rTree)
  where
    lCount = (n - 1) `div` 2
    rCount = n - 1 - lCount
    (lH, lTree') = buildTree lCount
    (rH, rTree) = buildTree rCount
    lTree = if lH < rH then redToBlack lTree' else lTree'

    redToBlack (T Red l v r) = T Black l v r
    redToBlack _ = error "violation found."

    rootColor =
        if color lTree == Red || color rTree == Red
          then Black
          else Red

experiment :: Int -> IO ()
experiment n = buildTree n `deepseq` pure ()

main :: IO ()
main = mapM_ experiment [1..1000]
