module Matrix
  ( saddlePoints
  ) where

import Data.Array

type Coord = (Int, Int)

saddlePoints :: Array Coord Int -> [Coord]
saddlePoints m = [ (rInd, cInd)
                 | (rInd, v1) <- rowMaxs
                 , (cInd, v2) <- colMins
                 , v1 == v2
                 ]
  where
    -- let's just assume lower bound is less than / equal to the upper bound
    -- in which case we know the array is not empty
    ((rowMin, colMin), (rowMax,colMax)) = bounds m

    rowInds = [rowMin .. rowMax]
    colInds = [colMin .. colMax]

    getRow rInd = map (\cInd -> m ! (rInd, cInd)) rowInds
    getCol cInd = map (\rInd -> m ! (rInd, cInd)) colInds

    rowMaxs = map (\rInd -> (rInd, maximum (getRow rInd))) rowInds
    colMins = map (\cInd -> (cInd, minimum (getCol cInd))) colInds
