module Problem97 where

{-

  I think a brute-force search with candidate list for each unsolved cells
  should be enough.

  - represent solved puzzle as a (Int,Int) -> Int mapping,
    I guess array-like stuff will add some unnecessary overhead.

  - unsolved puzzle represented as a (Int,Int) -> IntSet mapping,
    with each row / col / box as its own IntSet of missing numbers.

-}

import qualified Data.Map as M
import qualified Data.IntSet as IS

type Coord = (Int, Int)

type Solved = M.Map Coord Int
type Unsolved = M.Map Coord IS.IntSet

data MissingSets = MSets
  { msBoxes :: M.Map Int IS.IntSet
  , msRows :: M.Map Int IS.IntSet
  , msCols :: M.Map Int IS.IntSet
  }
