{-# LANGUAGE TupleSections #-}
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
import Data.Foldable
import Data.Char
import Data.Maybe
import Data.Either
import Control.Monad
import Data.List

type Coord = (Int, Int)

type Solved = M.Map Coord Int
type Unsolved = M.Map Coord IS.IntSet
type CellContent = Either Int IS.IntSet

-- a pack of nine things
type NinePack = []
type NinePackCoord = NinePack Coord

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
        Right $ fromMaybe
                  (error $ "getCell: missing cell " ++ show coord)
                  (M.lookup coord mUnsol)

-- TODO: normalize when putting values into the cell?
setCell :: Puzzle -> Coord -> CellContent -> Puzzle
setCell (pz@(mSol, mUnsol)) coord newCT = case getCell pz coord of
    Left _ -> case newCT of
        Left i -> (M.insert coord i mSol, mUnsol)
        Right s ->
            -- left to right
            (M.delete coord mSol, M.insert coord s mUnsol)
    Right _ -> case newCT of
        Left i ->
            -- right to left, very unlikely case.
            (M.insert coord i mSol, M.delete coord mUnsol)
        Right s ->
            (mSol, M.insert coord s mUnsol)

getRowCoords :: Int -> NinePackCoord
getRowCoords r = map (r,) ints

getColCoords :: Int -> NinePackCoord
getColCoords c = map (,c) ints

getBoxCoords :: Int -> NinePackCoord
getBoxCoords b = [(rBase+r,cBase+c) | r<-[0..2], c<-[0..2]]
  where
    (rBase,cBase) = [(r,c) | r <- [1,4,7], c <- [1,4,7]] !! (b-1)


npRemoveSolved :: NinePack CellContent -> NinePack CellContent
npRemoveSolved cells = updatedCells
  where
    solvedNums = IS.fromList $ lefts cells
    updatedCells :: [CellContent]
    updatedCells = map (either Left updateCandidates) cells
      where
        updateCandidates candSet = if IS.size s1 == 1
            then Left . head . IS.toList $ s1
            else Right s1
          where
            s1 = candSet `IS.difference` solvedNums

npLoneMissing :: NinePack CellContent -> NinePack CellContent
npLoneMissing cells = updatedCells
  where
    solvedNums = IS.fromList $ lefts cells
    missingNums = IS.toList $ IS.fromList ints `IS.difference` solvedNums
    updatedCells = foldl' (flip tryLonelyMissingNum) cells missingNums
    tryLonelyMissingNum :: Int -> [CellContent] -> [CellContent]
    tryLonelyMissingNum n curCells = if isLonely
        then
             let update cell flg = if flg then Left n else cell
             in zipWith update curCells checkPos
        else curCells
      where
        checkPos = map (either (const False) (\s -> n `IS.member` s)) curCells
        isLonely = case filter id checkPos of
            [_] -> True
            _ -> False

-- TODO: chained candidate elimination might help, don't know for now.

updatePuzzle :: Puzzle
             -> NinePackCoord
             -> NinePack CellContent -> NinePack CellContent
             -> Puzzle
updatePuzzle pz coords cells updatedCells =
    foldl' update pz (zip coords (zip cells updatedCells))
  where
    update curPz (coord,(oldCtnt,newCtnt)) = case oldCtnt of
        Left _ -> curPz
        Right _ -> setCell curPz coord newCtnt

-- given a pack of nine coordinates, update candidate lists of corresponding
-- cells. the update will fail if any cell gets an empty list of candidates
updateNinePack :: Puzzle -> NinePackCoord
               -> (NinePack CellContent -> NinePack CellContent)
               -> Maybe Puzzle
updateNinePack pz coords npUpdate = if hasEmpties then Nothing else Just updatedPuzzle
  where
    cells = map (getCell pz) coords
    updatedCells = npUpdate cells
    updatedPuzzle = updatePuzzle pz coords cells updatedCells
    hasEmpties = any IS.null (rights updatedCells)

cleanupCandidates :: Puzzle -> Maybe Puzzle
cleanupCandidates pz = do
    newPz <- onePass pz
    if newPz == pz
       then pure newPz
       else cleanupCandidates newPz
  where
    allNinePacks =
           map getRowCoords ints
        ++ map getColCoords ints
        ++ map getBoxCoords ints
    onePass curPz = foldM combinedUpdate curPz allNinePacks
    combinedUpdate pz' coords =
        updateNinePack pz' coords npRemoveSolved
        >>= \pz2 -> updateNinePack pz2 coords npLoneMissing

pprPuzzle :: Puzzle -> String
pprPuzzle pz = unlines (concatMap pprRow ints)
  where
    pprRow r =
        map (intercalate "|")
      . transpose
      $ map (\c -> pprCell $ getCell pz (r,c)) ints
    pprCell (Left i) = ["   ", " " ++ show i ++ " ", "   "]
    pprCell (Right s) = (map . map) f [[1,2,3],[4,5,6],[7,8,9]]
      where
        f x = if IS.member x s then chr (x + ord '0') else ' '

mkPuzzle :: RawIntArray -> Puzzle
mkPuzzle raw = partitioned
  where
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

solvePuzzle :: Puzzle -> Maybe Puzzle
solvePuzzle = cleanupCandidates >=> solvePuzzle'
  where
    solvePuzzle' (pz@(_,mUnsol))
        | M.null mUnsol = pure pz
        | otherwise = do
            -- number of candidates -> coordinate set
            let candidateMap :: M.Map Int [Coord]
                candidateMap = M.fromListWith (++) .
                    map (\(coord,cSet) -> (IS.size cSet, [coord])) $ M.toList mUnsol
                curCoord = head . snd . head $ M.toAscList candidateMap
                candidates :: [Int]
                candidates = IS.toList . fromJust $ M.lookup curCoord mUnsol
            listToMaybe $ do
                i <- candidates
                maybeToList (solvePuzzle (setCell pz curCoord (Left i)))

