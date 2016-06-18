{-# LANGUAGE ScopedTypeVariables #-}
module Problem92 where

import Control.Arrow
import Data.List
import Data.Maybe
import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use uncurry" #-}

-- "pick" from Problem90
pick :: forall a. [a] -> [(a,[a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split :: ([a], [a]) -> (a,[a])
    split (ls,v:rs) = (v,ls++rs)
    split _ = error "cannot split empty list"

type Tree = IM.IntMap IS.IntSet
type Edge = (Int,Int)

addEdge :: Edge -> Tree -> Tree
addEdge (a,b) = insertEdge a b . insertEdge b a
  where
    insertEdge x y = IM.alter (Just . IS.insert x . fromMaybe IS.empty) y

neighbors :: Int -> Tree -> IS.IntSet
neighbors k m = fromMaybe IS.empty $ IM.lookup k m

uniqueInts :: [Int] -> Bool
uniqueInts xs = length xs == IS.size (IS.fromList xs)

type NodeAssigns = IM.IntMap Int

-- TODO:
-- - keep a set of not yet covered differences
-- - keep a list of unchecked edges, remove some accordingly when
--   new node is inserted.

-- t: tree, edges: all edges (cached result)
-- assigned: current node number assignments
-- remainings: not-yet-assigned numbers
-- todo: nodes not visited
-- cur: the node we are looking at
search :: Tree -> [Edge] -> NodeAssigns -> [Int] -> IS.IntSet -> [NodeAssigns]
search t edges assigned remainings todo
    | IS.null todo = do
        let edgeDiffs = mapMaybe (getDiff assigned) edges
        guard (length edges == length edgeDiffs
            && and (zipWith (==) (sort edgeDiffs) [1..]))
        pure assigned
    | otherwise = do
        let (cur,newTodo) = fromJust $ IS.minView todo
        (newNum, newRemainings) <- pick remainings
        let newAssigned = IM.insert cur newNum assigned
            edgeDiffs = mapMaybe (getDiff newAssigned) edges
        guard $ uniqueInts edgeDiffs
        search t edges newAssigned newRemainings newTodo
  where
    getDiff curAssigned (x,y) =
        fmap abs (subtract
                  <$> IM.lookup x curAssigned
                  <*> IM.lookup y curAssigned)

search2 :: Tree -> [Edge] -> IS.IntSet -> NodeAssigns -> [Int] -> IS.IntSet -> [NodeAssigns]
search2 t missingEdges missingDiffs assigned remainings todo
    | IS.null todo = do
        guard (null missingEdges && IS.null missingDiffs)
        pure assigned
    | otherwise = do
        let (cur,newTodo) = fromJust $ IS.minView todo
        (newNum, newRemainings) <- pick remainings
        -- assign (cur, newNum)
        let newAssigned = IM.insert cur newNum assigned
            isRelated (a,b)
                | a == cur = isJust (IM.lookup b newAssigned)
                | b == cur = isJust (IM.lookup a newAssigned)
                | otherwise = False
            (relatedEdges, newMissingEdges) = partition isRelated missingEdges
            newDiffs = IS.fromList $ map (fromJust . getDiff newAssigned) relatedEdges
        guard $ IS.null $ missingDiffs `IS.intersection` newDiffs
        search2 t newMissingEdges (missingDiffs `IS.union` newDiffs) newAssigned newRemainings newTodo
  where
    getDiff curAssigned (x,y) =
        fmap abs (subtract
                  <$> IM.lookup x curAssigned
                  <*> IM.lookup y curAssigned)

solve :: [Edge] -> [NodeAssigns]
solve es = search2 t es (IS.fromList [1..l-1]) IM.empty nodes (IS.fromList nodes)
  where
    l = length es + 1
    nodes = [1..l]
    t = foldl' (flip addEdge) IM.empty es

vonKoch :: [Edge] -> [ [Int] ]
vonKoch = map convert . solve
  where
    -- convert one solution to the correct format
    convert = map snd . IM.toAscList
