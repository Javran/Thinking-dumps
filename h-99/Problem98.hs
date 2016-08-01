{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds, TupleSections #-}
module Problem98 where

import Control.Monad
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Function
import qualified Data.Array.IArray as Arr
import qualified Data.Map.Strict as M

-- this simple algorithm suffers when the puzzle becomes large.
-- this is because our algorithm either fully fills a line (row / col)
-- or do nothing if it finds something inconsistent. but a more efficent
-- strategy is to just partially solve a line as much as it can.
-- (e.g. if a line has 10 cells, and we need 8 consecutive black cells,
-- then no matter what the 6 cells in the middle have to be painted black)
-- and this strategy is exactly what we need in order to solve advanced puzzles.
-- but I think I'm good with current solution as long as it can solve the example puzzle
-- quickly (which it does).
-- for anyone who is interested in more advanced solving technique,
-- you might try solving puzzles in https://github.com/mikix/nonogram-db .
-- I originally planned using few from it as testcases,
-- but I eventually dropped this idea as most of them will take forever for my solver
-- to work it out.

data Rule = Rule
  { ruleLens :: [Int] -- lengths, all numbers should be greater than 0
    -- calculated from ruleLens, the length of the most compact solution
    -- satisfying this rule.
  , ruleAtLeast :: !Int
  } deriving (Show)

-- "RCRule" (Row/Col Rule) is a rule for a line with index of that line.
-- Left index : Rule for Row "index"
-- Right index: Rule for Col "index"
type RCRule = (Either Int Int, Rule)
-- cell content for an unsolved puzzle.
-- Nothing: not yet filled of anything
-- Just True: this cell is painted black.
-- Just False: this cell is painted white.
type CellContent = Maybe Bool
-- the description of a nonogram, including # of rows and # of cols.
-- and a complete list of rules (paired with line index)
data Nonogram = NG !Int !Int [RCRule]

data RectElemState = Solved | Unsolved

-- just a fancy way of saying "Bool" and "Maybe Bool",
-- depending on whether we are representing a solved or unsolved puzzle
type family RectElem (a :: RectElemState)

type instance RectElem 'Solved = Bool
type instance RectElem 'Unsolved = Maybe Bool

-- the "Rect a" represent a partial or complete solution of puzzle
type Rect a = Arr.Array (Int,Int) (RectElem a)

-- traverse the list, separate the minimum element with rest of the list,
-- it's guaranteed that the ordering is preserved.
minViewBy :: (a -> a -> Ordering) -> [a] -> Maybe (a,[a])
minViewBy _ [] = Nothing
minViewBy f xs = Just . minimumBy (f `on` fst) $ xsWithContext
  where
    xsWithContext = zip xs (zipWith (++)
                              (init $ inits xs)
                              (tail $ tails xs))

mkRule :: [Int] -> Rule
mkRule [] = Rule [] 0
mkRule xs = Rule xs $ sum xs + length xs - 1

mkRowRule, mkColRule :: Int -> [Int] -> RCRule

mkRowRule i xs = (Left i, mkRule xs)
mkColRule i xs = (Right i, mkRule xs)

-- a rule consists of a list of numbers, "ruleView r" destructs a rule like how
-- "minView" destructs an (usually ordered) data structure to separate one element
-- from rest of it.
ruleView :: Rule -> Maybe ((Int, Int), Rule)
ruleView (Rule [] _) = Nothing
ruleView (Rule [x] l) = Just ((x,l), Rule [] 0)
ruleView (Rule (x:xs) l) = Just ((x,l), Rule xs (l-x-1))

-- given all alternatives of a line,
-- determine if there are cells that has to be of one particular value (True/False)
-- e.g.
-- > mergeResult [N,N,N] [ [T,T,F], [F,T,F] ] = [N, J T, J F]
-- > mergeResult [N,N,N] [ [T,T,F], [F,T,T] ] = [N, J T, N]
-- the first parameter can be a list of Nothing.
-- and the length of both parameter must be the same.
-- when the corresponding element in first parameter is a "Just _"
-- the merging process will be short-cutted to save some time.
mergeResults :: [ CellContent ] -> [ [Bool] ] -> [ CellContent ]
mergeResults =
    zipWith
      (\cell alts ->
       getFirst $ ((<>) `on` First) cell (allEq alts))
  where
    allEq [] = Nothing
    allEq (a:as) = guard (all (== a) as) >> Just a

-- given a line rule and corresponding contents,
-- "solveRule" tries to give us all possible complete solutions of this line
solveRule :: Rule -> [CellContent] -> [ [Bool] ]
solveRule r1 xs1 = map tail (($ []) <$> solveRule' r1 (Nothing:xs1) ([] ++))
  where
    -- when "solveRule'" tries to satisfy a rule, it always paint a white cell
    -- before painting consecutive blacks. by doing so the logic can be simplified a bit.
    -- this is why we always prepend "Nothing" and just take the "tail" of every result
    -- when calling this function from "solveRule"
    -- the type signature of "acc" looks a bit funny: you might expect it to be [Bool]
    -- because it's a list of Bools, but the type is really [Bool] -> [Bool].
    -- this is what we call difference list, and it's known to have better performance
    -- when concatenating lists than simply using (++).
    -- see: https://wiki.haskell.org/Difference_list for help
    solveRule' :: Rule -> [CellContent] -> ([Bool] -> [Bool]) -> [ [Bool] -> [Bool] ]
    solveRule' r xs acc = case ruleView r of
        Nothing ->
            -- all rules have been satisfied, we fill rest of the cells with False
            ((\zs -> acc . (zs ++)) . fst) <$>
                maybeToList (checkedFill False (length xs) xs 0)
        -- now we are trying to have one or more "False" and "curLen" consecutive "True"s
        Just ((curLen,leastL), r') -> do
            -- we can fail immediately here if we have insufficient number of cells.
            guard $ length xs >= leastL + 1
            -- always begin with one "False"
            (filled1,remained1) <- maybeToList $ checkedFill False 1 xs 0
            -- now we have 2 options:
            -- either (1) start filling in these cells immediately (fillNow)
            -- or we just delay the filling a bit, paint next cell white
            -- and let the recursive call handle rest of the problem.
            -- this allows us to fill one or more white cells before painting black cells.
            let acc' = acc . (filled1 ++)
                fillNow = do
                   (filled2, remained2) <- maybeToList $ checkedFill True curLen remained1 0
                   solveRule' r' remained2 $ acc' . (filled2 ++)
                fillLater = solveRule' r remained1 acc'
            fillNow ++ fillLater
    -- "checkedFill b count ys" tries to fill "count" number of Bool value "b"
    -- into cells, results in failure if cell content cannot match with the indended value.
    checkedFill :: Bool -> Int -> [CellContent] -> Int -> Maybe ([Bool], [CellContent])
    checkedFill b count ys countFilled
        | count == 0 =
            -- no need to fill in anything, done.
            -- there's no need reversing the list, as all values filled in
            -- are the same.
            pure (take countFilled bs, ys)
        | otherwise = case ys of
            [] ->
                -- there's no room for filling anything, results in failure
                mzero
            (m:ys') -> do
                -- if the cell has not yet been filled, there's no problem.
                -- otherwise the already-existing value should match what
                -- we are filling in.
                guard (maybe True (\b2 -> b == b2) m)
                checkedFill b (count-1) ys' $! countFilled+1
      where
        bs = repeat b

-- make a Rectangle for starting working on the puzzle,
-- all cells are initialized to Nothing
mkRect :: Int -> Int -> Rect 'Unsolved
mkRect nRow nCol = Arr.array ((1,1), (nRow,nCol)) vals
  where
    vals = zip
             [(r,c) | r <- [1..nRow], c <- [1..nCol]]
             (repeat Nothing)

{-
  - for each rule, we grab the corresponding cells, and use "solveRule" to
    get a list of all possible solutions, more solutions mean more flexible.
  - to prevent "solveRule" from giving too many alternatives, we can use "take"
    to give an upper bound about this "flexibility": say the upper bound
    is 100, then "flexibility" of a rule (row / col) cannot exceed 100.
  - when the definition of "flexibility" results in a tie,
    the original one (total length - min required number of cells) is compared.
-}
solveRect :: Nonogram -> Maybe (Rect 'Solved)
solveRect (NG nRow nCol rs) = solveRect' (mkRect nRow nCol) rs
  where
    -- measure "flexibility" of a rule, the less flexible a rule is,
    -- the less solutions it can have for one certain row / col
    flex :: RCRule -> Int
    flex (lr, Rule _ atLeast) = case lr of
        Left _ -> nRow - atLeast
        Right _ -> nCol - atLeast

    solveRect' :: Rect 'Unsolved -> [RCRule] -> Maybe (Rect 'Solved)
    solveRect' curRect rules = case minViewBy (compare `on` snd . snd) processedRules of
        Nothing ->
            -- all rules are considered, we now check whether the puzzle is fully solved.
            checkRect curRect
        Just (((lr,_),(solutions, _)),rules') -> listToMaybe $ do
            -- pick up one with minimum flexibility
            let indices = getIndices lr
            -- pick one solution
            solution <- solutions
            let newAssocs = zip indices (map Just solution)
                newRect = Arr.accum (\_old new -> new) curRect newAssocs
            -- apply it to the rectangle (the puzzle)
            maybeToList $ solveRect' newRect (map fst rules')
      where
        getIndices lr = case lr of
            Left  rowInd -> map (rowInd,) [1..nCol]
            Right colInd -> map (,colInd) [1..nRow]
        -- return type: (row/col index, (all solutions, (bounded length of solutions, flex)))
        processRule :: RCRule -> (RCRule, ([[Bool]], (Int, Int)))
        processRule r@(lr,rule) = (r, (solutions,(estSearchSpace, flex r)))
          where
            searchCap = 10 :: Int
            indices = getIndices lr
            extracted = map (curRect Arr.!) indices
            solutions = solveRule rule extracted
            estSearchSpace = length (take searchCap solutions)
        processedRules = map processRule rules

    -- turn every cell into "solved" cells only when all cells are filled.
    checkRect :: Rect 'Unsolved -> Maybe (Rect 'Solved)
    checkRect ar = do
        guard $ all isJust (Arr.elems ar)
        pure $ Arr.amap fromJust ar

fromRawNonogram :: [[Int]] -> [[Int]] -> Nonogram
fromRawNonogram rowRules colRules = NG (length rowRules) (length colRules) rules
  where
    rowRules' = zipWith (\rInd raw -> (Left rInd, mkRule raw)) [1..] rowRules
    colRules' = zipWith (\rInd raw -> (Right rInd, mkRule raw)) [1..] colRules
    rules = rowRules' ++ colRules'

-- create a pretty-printing-ready string for solved nonograms
pprSolvedNonogram :: Nonogram -> Rect 'Solved -> String
pprSolvedNonogram (NG nRow nCol rules') rect = unlines (map pprRow [1..nRow] ++ pprdColRules)
  where
    lookupRule k = M.lookup k rules
    rules = M.fromList rules'

    getRawRule (Rule rs _) = rs

    pprdColRules :: [String]
    pprdColRules = map ((' ':) . unwords . map toStr) tr
      where
        colRules = map (maybe [] getRawRule . lookupRule . Right) [1..nCol]
        longest = maximum (map length colRules)
        paddedRules = map (take longest . (++ repeat 0)) colRules
        tr = transpose paddedRules

        toStr 0 = " "
        toStr n = show n

    pprRow :: Int -> String
    pprRow rInd = '|' : intercalate "|" cells ++ "| " ++ unwords (map show rowRule)
      where
        rowRule = maybe [] getRawRule $ lookupRule (Left rInd)
        cells = map (toStr . (rect Arr.!) . (rInd,)) [1..nCol]
        toStr False = "_"
        toStr True = "X"

nonogram :: [[Int]] -> [[Int]] -> String
nonogram rowRules colRules =
    maybe "No solution.\n" (pprSolvedNonogram ng) (solveRect ng)
  where
    ng = fromRawNonogram rowRules colRules

{-
  TODO:
  - testcases.
  - set a threshold for merging:
    when there are only few alternatives
    instead of trying to completely fill in
    one line, we get all alternatives and
    try to find some "have to be" cells.
    so we will have 2 thresholds: one for terminating
    alternative enumeration, and another for merging when
    the number of alternatives is really small (at least it should
    be smaller than the terminating threshold.
-}
