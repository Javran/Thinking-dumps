module Problem97 where

{-

  I think a brute-force search with candidate list for each unsolved cells
  should be enough.

  - represent solved puzzle as a (Int,Int) -> Int mapping,
    I guess array-like stuff will add some unnecessary overhead.

  - unsolved puzzle represented as a (Int,Int) -> IntSet mapping,
    with each row / col / box as its own IntSet of missing numbers.

-}

import qualified Data.Map.Strict as M
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM
import Data.Foldable
import Data.Monoid
import Data.Char
import Data.Maybe

type Coord = (Int, Int)

type Solved = M.Map Coord Int
type Unsolved = M.Map Coord IS.IntSet
type NineCells = IM.IntMap IS.IntSet
type CellContent = Either Int IS.IntSet

data MissingSets = MSets
  { msBoxes :: NineCells
  , msRows :: NineCells
  , msCols :: NineCells
  }

type Puzzle = (Solved, Unsolved)

type RawIntArray = String

ints :: [Int]
ints = [1..9]

getCell :: Puzzle -> Coord -> CellContent
getCell (mSol, mUnsol) coord = case M.lookup coord mSol of
    Just i -> Left i
    Nothing ->
        -- we want to make sure it is safe reducing it to WHNF
        -- so even when the Puzzle's Unsolved part is missing
        -- we can get something out without runtime error
        Right $ fromJust (M.lookup coord mUnsol)

getRow :: Puzzle -> Int -> [CellContent]
getRow pz r = map (\c -> getCell pz (r,c)) ints

getCol :: Puzzle -> Int -> [CellContent]
getCol pz c = map (\r -> getCell pz (r,c)) ints

getBox :: Puzzle -> Int -> [CellContent]
getBox pz b = map (getCell pz) [(rBase+r,cBase+c) | r<-[0..2], c<-[0..2]]
  where
    (rBase,cBase) = [(r,c) | r <- [1,4,7], c <- [1,4,7]] !! (b-1)

mkPuzzle :: RawIntArray -> Puzzle
mkPuzzle raw = undefined
  where
    msEmpty = MSets mempty mempty mempty
    -- parsed sudoku with coordinates
    withCoords = zip [(x,y) | x<-ints,y<-ints] (map p raw)
      where
        p :: Char -> Int
        p x = ord x - ord '0'
    -- sudoku cells partitioned into 2 sets.
    partitioned :: Puzzle
    partitioned = foldl' insertCell mempty withCoords
      where
        allCandidates = IS.fromList ints
        insertCell (mSol,mUnsol) (coord,i) = case i of
            0 -> (mSol, M.insert coord allCandidates mUnsol)
            _ -> (M.insert coord i mSol, mUnsol)
