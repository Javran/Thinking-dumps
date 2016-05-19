module Ch03Exercise9 where

import Ch03RedBlack

buildTree :: Int -> (Int, Tree ())
buildTree 0 = (0, E)
buildTree 1 = (0, T Red E () E)
buildTree 2 = (1, T Black (T Red E () E) () E)
buildTree n = (if rootColor == Red then rH else rH+1, T rootColor lTree () rTree)
  where
    lCount = (n - 1) `div` 2
    rCount = n - 1 - lCount
    (lH, lTree) = buildTree lCount
    (rH, rTree') = buildTree rCount
    rTree = if lH < rH then redToBlack rTree' else rTree'

    redToBlack (T Red l v r) = T Black l v r
    redToBlack _ = error "violation found."

    rootColor =
        if color lTree == Red || color rTree == Red
          then Black
          else Red
