{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing #-}

module Main ( main, test {-, maxDistances -} ) where

import Prelude
import System.Environment
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Interpreter
import AccelerateCompat as A

-- <<Graph
type Weight = Int32
type Graph = Array DIM2 Weight
-- >>

-- -----------------------------------------------------------------------------
-- shortestPaths

-- <<shortestPaths
shortestPaths :: Graph -> Graph
shortestPaths g0 = run (shortestPathsAcc n (use g0))
  where
    Z :. _ :. n = arrayShape g0
-- >>

shortestPathsAcc :: Int -> Acc Graph -> Acc Graph
shortestPathsAcc n =
    -- (>->) is just function composition but to provide the library
    -- a little bit hint for optimization
    foldl1 (>->) steps
 where
  -- gradually count to n (take n steps)
   -- this corresponds to the outermost loop in a standard Floyd-Warshall algorithm
  steps :: [ Acc Graph -> Acc Graph ]
  steps =  [ step (unit (constant k)) | k <- [0 .. n-1] ]

step :: Acc (Scalar Int) -> Acc Graph -> Acc Graph
step k g =
    -- make an array of the same shape
    generate (shape g) sp
 where
   -- k is just k' wrapped inside an array
   -- for some performance issue, we'd better pass arrays around
   -- when doing a series of array operations
   k' = the k

   -- unpack index, update as necessary
   sp :: Exp DIM2 -> Exp Weight
   sp ix = let (Z :. i :. j) = unlift ix
           in A.min (g ! index2 i j)
                    (g ! index2 i k' + g ! index2 k' j)

-- -----------------------------------------------------------------------------
-- Testing

-- <<inf
inf :: Weight
inf = 999
-- >>

testGraph :: Graph
testGraph = toAdjMatrix
        [[  0, inf, inf,  13, inf, inf],
         [inf,   0, inf, inf,   4,   9],
         [ 11, inf,   0, inf, inf, inf],
         [inf,   3, inf,   0, inf,   7],
         [ 15,   5, inf,   1,   0, inf],
         [ 11, inf, inf,  14, inf,   0]]

-- correct result:
expectedResult :: Graph
expectedResult = toAdjMatrix
         [[0,  16, inf, 13, 20, 20],
          [19,  0, inf,  5,  4,  9],
          [11, 27,   0, 24, 31, 31],
          [18,  3, inf,  0,  7,  7],
          [15,  4, inf,  1,  0,  8],
          [11, 17, inf, 14, 21,  0] ]

test :: Bool
test = toList (shortestPaths testGraph) == toList expectedResult

toAdjMatrix :: [[Weight]] -> Graph
toAdjMatrix xs = A.fromList (Z :. k :. k) (concat xs)
  where k = Prelude.length xs


main :: IO ()
main = do
    -- uncomment the following line to see if the test works ..
    -- it should print out "True"
    -- print test
    -- read the first argument and read it as a number
    -- I think the original impl is little overcomplicated
    -- so now it's refactored for readability
    n <- read . head <$> getArgs
    -- generate a graph to work with,
    -- the size of the graph seems to be n x n, where n is the number of vertices
    print (run (let g :: Acc Graph
                    -- specify size
                    g = generate (constant (Z:.n:.n) :: Exp DIM2) f
                    -- specify element at each index
                    f :: Exp DIM2 -> Exp Weight
                    -- looks like an edge from i to j has the length / weight
                    -- of j+i*n ... I guess it's just a random choice
                    f ix = let i,j :: Exp Int
                               Z:.i:.j = unlift ix
                           in A.fromIntegral j +
                              A.fromIntegral i * constant (Prelude.fromIntegral n)
                in A.foldAll (+) (constant 0) (shortestPathsAcc n g)))
