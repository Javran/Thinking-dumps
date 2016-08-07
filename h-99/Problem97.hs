{-# LANGUAGE TupleSections #-}
module Problem97 where

{-

  NOTE: about the design of this solver:

  - as many other sudoku solvers, it uses candidate numbers.
    that means a cell contains either a single number, or a
    set of possible numbers, we gradually remove impossible numbers
    until reach a single number for all cells in the puzzle

  - we first go through some known strategies of eliminating candidates,
    this includes:
    - removing already-filled numbers from candidate list
    - try to find "lone numbers": if one not-yet filled number
      appears in exactly one of the NinePack cells, then
      this "lone" number has to be put on the cell in question.
    - consider pack of nine cells a partial sudoku, enumerate all of the outcome
      and then figure out the real candidate for each cells.
      (see "npSolveNinePack'" for detail)
      note that this is a really expensive strategy so to make it more useful
      we should only try this as the last resort, when none of the simple
      strategies can make any progress.
  - then those strategies are applied alternatively until we cannot get any cell updated
    (reaching a "fixpoint")
  - obviously we don't expect these simple strategies to work for all puzzles,
    so in this step we apply a search with simple heuristic: we always prioritize
    on cells that has as few candidates as possible. intuitively this reduces the
    branching factor.
  - after making the decision of filling one cell, we go back to the original
    strategy-based algorithm, hoping to eliminate more candidates.
    then the search repeats until a complete solution is found.

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

-- a Puzzle is a mapping from coordinates to corresponding cell's content.
-- there are 2 types of content: when the cell has filled with a concrete, single number,
-- we consider this cell to be "solved", or when the cell has more than one candidate
-- number, in which case we call it "unsolved"
-- a Puzzle has two parts: "Solved" maps solved cells to their numbers,
-- and "Unsolved" maps unsolved cells to the set of candidate numbers.
-- a proper Puzzle should contain mappings for all coordinates in either
-- of these 2 sets and every coordinate should occur exacly once.
type Puzzle = (Solved, Unsolved)

-- we consider a string of digits '0' .. '9' as "RawIntArray",
-- where '0' represents empty cells and other represents solved cells.
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

-- NOTE: seems the simplified version doesn't do good to the program.
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

{-
  "npSolveNinePack' szThres np" tries to solve a pack of nine cells
  as if it was a "partial sudoku": we expand every possible combinations of cells,
  and summarize the result to eliminate candidates that are actually impossible.

  I think doing so has the effect of eliminating candidates using
  naked/hidden pairs/triples.

  However, this is a brute force approach after all and should not be used
  too often otherwise the performance will definitely be impairred.
-}
npSolveNinePack' :: Int -> NinePack CellContent -> NinePack CellContent
npSolveNinePack' sizeThres nps = if univSize <= sizeThres
    then zipWith update (transpose univ) nps
    else nps
  where
    univSize = product (map f nps)
      where
        f (Left _) = 1
        f (Right s) = IS.size s
    update cans c = case c of
        Left _ -> c
        Right _ -> Right (IS.fromList cans)

    -- try all possibilities
    partialUniv :: [CellContent] -> [ [Int] ]
    partialUniv [] = pure []
    partialUniv (Left i:xs) = (:) <$> pure i <*> partialUniv xs
    partialUniv (Right s:xs) = do
        i <- IS.toList s
        let upd c = case c of
                Left _ -> pure c
                Right s1 -> do
                    let s' = IS.delete i s1
                    guard $ not . IS.null $ s'
                    pure $ Right s'
        xs' <- mapM upd xs
        result <- partialUniv xs'
        pure (i : result)

    univ = partialUniv nps

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

cleanupCandidates :: Maybe Int -> Puzzle -> Maybe Puzzle
cleanupCandidates deepClean pz = do
    newPz <- onePass pz
    if newPz == pz
       then case deepClean of
         Nothing -> pure newPz
         Just threshold ->
             deepCleanup threshold newPz
       else cleanupCandidates deepClean newPz
  where
    allNinePacks =
           map getRowCoords ints
        ++ map getColCoords ints
        ++ map getBoxCoords ints
    onePass curPz = foldM combinedUpdate curPz allNinePacks
    combinedUpdate pz' coords =
        updateNinePack pz' coords npRemoveSolved
        >>= \pz2 -> updateNinePack pz2 coords npLoneMissing
    deepCleanup threshold pz' = do
        newPz' <- foldM (\curPz coords ->
                         updateNinePack curPz coords (npSolveNinePack' threshold))
                        pz' allNinePacks
        if newPz' == pz'
           then pure newPz'
           else cleanupCandidates deepClean newPz'

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
solvePuzzle = cleanupCandidates Nothing >=> solvePuzzle'
-- the following code enables "deep clean strategy", which is to allow
-- expensive strategy "solveNinePack" to run when all those simple
-- strategies can't make any progress.
-- solvePuzzle = cleanupCandidates (Just 100) >=> solvePuzzle'
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

