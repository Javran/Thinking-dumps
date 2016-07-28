{-# LANGUAGE FlexibleContexts, TypeFamilies, DataKinds, TupleSections #-}
module Problem98 where

import Control.Monad
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Array.IArray as Arr
import qualified Data.Map.Strict as M

data Rule = Rule
  { ruleLens :: [Int] -- lengths
    -- calculated from ruleLens, the length of the most compact solution
    -- satisfying this rule.
  , ruleAtLeast :: !Int
  } deriving (Show)

-- Left: Row rule
-- Right: Col rule
type RCRule = (Either Int Int, Rule)
type CellContent = Maybe Bool
data Nonogram = NG !Int !Int [RCRule]

data RectElemState = Solved | Unsolved

type family RectElem (a :: RectElemState)

type instance RectElem 'Solved = Bool
type instance RectElem 'Unsolved = Maybe Bool

type Rect a = Arr.Array (Int,Int) (RectElem a)

minViewBy :: (a -> a -> Ordering) -> [a] -> Maybe (a,[a])
minViewBy _ [] = Nothing
minViewBy f xs = Just . minimumBy (f `on` fst) $ xsWithContext
  where
    xsWithContext = zip xs (zipWith (++)
                              (init $ inits xs)
                              (tail $ tails xs))

mkRule :: [Int] -> Rule
mkRule xs = Rule xs $ sum xs + length xs - 1

mkRowRule, mkColRule :: Int -> [Int] -> RCRule

mkRowRule i xs = (Left i, mkRule xs)
mkColRule i xs = (Right i, mkRule xs)

ruleView :: Rule -> Maybe ((Int, Int), Rule)
ruleView (Rule [] _) = Nothing
ruleView (Rule [x] l) = Just ((x,l), Rule [] 0)
ruleView (Rule (x:xs) l) = Just ((x,l), Rule xs (l-x-1))

solveRule :: Rule -> [CellContent] -> [ [Bool] ]
solveRule r1 xs1 = map tail (solveRule' r1 (Nothing:xs1))
  where
    -- TODO: let's say to satisfy the next new rule
    -- we always fill in a "False" as the separator
    -- and caller of this function should be responsible
    -- for prepending a Nothing in front of the [CellContent]
    solveRule' :: Rule -> [CellContent] -> [ [Bool] ]
    solveRule' r xs = case ruleView r of
        -- all rules have been satisfied, we fill rest of the cells with False
        Nothing -> fst <$> checkedFill False (length xs) xs
        -- now we are trying to have one or more "False" and "curLen" consecutive "True"s
        Just ((curLen,leastL), r') -> do
            -- we can fail immediately here if we have insufficient number of cells.
            guard $ length xs >= leastL + 1
            -- always begin with one "False"
            (filled1,remained1) <- checkedFill False 1 xs
            -- now we have 2 options, either start filling in these cells, or
            startFill <- [True, False]
            if startFill
                then do
                    (filled2, remained2) <- checkedFill True curLen remained1
                    filled3 <- solveRule' r' remained2
                    pure (filled1 ++ filled2 ++ filled3)
                else do
                    filled2 <- solveRule' r remained1
                    pure (filled1 ++ filled2)
    -- "checkedFill b count ys" tries to fill "count" number of Bool value "b"
    -- into cells, results in failure if cell content cannot match with the indended value.
    checkedFill :: Bool -> Int -> [CellContent] -> [ ([Bool], [CellContent]) ]
    checkedFill b count ys
        | count == 0 =
            -- no need to fill in anything, done.
            pure ([], ys)
        | otherwise = case ys of
            [] ->
                -- there's no room for filling anything, results in failure
                mzero
            (m:ys') -> do
                -- if the cell has not yet been filled, there's no problem.
                -- otherwise the already-existing value should match what
                -- we are filling in.
                guard (maybe True (\b2 -> b == b2) m)
                (filled, remained) <- checkedFill b (count-1) ys'
                pure (b:filled, remained)

mkRect :: Int -> Int -> Rect 'Unsolved
mkRect nRow nCol = Arr.array ((1,1), (nRow,nCol)) vals
  where
    vals = zip
             [(r,c) | r <- [1..nRow], c <- [1..nCol]]
             (repeat Nothing)

{-
  TODO: for now the "flexibility" does not change throughout
  our search, which I believe can be optimized:

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
        Nothing -> checkRect curRect
        Just (((lr,_),(solutions, _)),rules') -> listToMaybe $ do
            let indices = getIndices lr
            solution <- solutions
            let newAssocs = zip indices (map Just solution)
                newRect = Arr.accum (\_old new -> new) curRect newAssocs
            maybeToList $ solveRect' newRect (map fst rules')
      where
        getIndices lr = case lr of
            Left  rowInd -> map (rowInd,) [1..nCol]
            Right colInd -> map (,colInd) [1..nRow]
        -- return type: (row/col index, (all solutions, (bounded length of solutions, flex)))
        processRule :: RCRule -> (RCRule, ([[Bool]], (Int, Int)))
        processRule r@(lr,rule) = (r, (solutions,(estSearchSpace, flex r)))
          where
            searchCap = 20 :: Int
            indices = getIndices lr
            extracted = map (curRect Arr.!) indices
            solutions = solveRule rule extracted
            estSearchSpace = length (take searchCap solutions)
        processedRules = map processRule rules

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


main :: IO ()
main = putStr $ nonogram [ [2,2,1,1,2,5,1], [1,1,4,3,3,1,1,1], [1,4,6,3,2], [9,1,1,7], [3,1,3,5,6], [4,1,2,2,3,4], [2,2,2,2,1,6], [2,5,5,1], [4,3,3,2,1],
                    [4,1,4,2,3], [2,4,6,1], [10,2,1,1,1,2], [3,1,2,1,9,1], [10,7,2,1], [1,10,2,6], [3,1,13,2], [14,3,5], [1,2,5,4,1,1,3],
                    [1,1,1,4,1,1,1,3], [9,2,2,1,2,3], [1,2,10,2,3], [1,2,1,1,2,4], [2,1,5,3,1,1], [2,8,6,1,1], [8,4,1,6]] [
                    [3,2,3,1,1], [1,4,2,4,2,2,3], [8,3,1,2,4], [1,4,3,1,4,3,1], [4,2,10,2], [3,2,2,5,1,3], [2,1,2,5,1,3,2], [1,3,2,10,3],
                    [5,4,1,2,5,3], [7,10,2], [1,1,5,5], [1,1,1,4,2,3], [3,3,1,3,3,1], [6,5,3,2,1], [1,1,14,1], [3,2,4,1,2,1],
                    [2,3,6,4,1,2], [3,3,1,2,2,3,1], [4,2,8,5], [1,3,5,4,1,4], [2,2,5,1,4,2,1,1], [5,7,1,1,1],
                    [7,1,2,5,3], [4,5,6,1], [1,3,1,1,1,1,3,1,2]]

{-
*Problem98> :set +m
*Problem98> putStr $ nonogram [ [3,5], [8], [3,2], [1,1,1], [1,3], [3,1], [2,1], [4,1], [2,3], [2,3] ] [
*Problem98|                     [3,4], [3,5], [3,1,1], [2,4], [1], [4,3], [3,2], [2,1,2], [1,1], [1,3] ]
|_|X|X|X|_|X|X|X|X|X| 3 5
|X|X|X|X|X|X|X|X|_|_| 8
|X|X|X|_|_|X|X|_|_|_| 3 2
|X|_|_|_|_|X|_|_|_|X| 1 1 1
|_|_|_|X|_|_|_|X|X|X| 1 3
|_|X|X|X|_|_|_|_|_|X| 3 1
|X|X|_|X|_|_|_|_|_|_| 2 1
|X|X|X|X|_|X|_|_|_|_| 4 1
|X|X|_|_|_|X|X|X|_|_| 2 3
|X|X|_|_|_|X|X|X|_|_| 2 3
 3 3 3 2 1 4 3 2 1 1
 4 5 1 4   3 2 1 1 3
     1         2
-}
