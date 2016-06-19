{-# LANGUAGE ScopedTypeVariables #-}
module Problem92 where

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

-- t: tree, edges: all edges (cached result)
-- assigned: current node number assignments
-- remainings: not-yet-assigned numbers
-- todo: nodes not visited
-- cur: the node we are looking at
search :: Tree -> [Edge] -> IS.IntSet -> NodeAssigns -> [Int] -> IS.IntSet -> [NodeAssigns]
search t missingEdges missingDiffs assigned remainings todo
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
            find' node = fromJust $ IM.lookup node newAssigned
            getDiff (x,y) = abs (subtract (find' x) (find' y))
            (relatedEdges, newMissingEdges) = partition isRelated missingEdges
            newDiffs = IS.fromList $ map getDiff relatedEdges
            newMergedDiffs = missingDiffs `IS.union` newDiffs
        guard $ IS.null $ missingDiffs `IS.intersection` newDiffs
        search t newMissingEdges newMergedDiffs newAssigned newRemainings newTodo

solve :: [Edge] -> [NodeAssigns]
solve es = search t es (IS.fromList [1..l-1]) IM.empty nodes (IS.fromList nodes)
  where
    l = length es + 1
    nodes = [1..l]
    t = foldl' (flip addEdge) IM.empty es

vonKoch :: [Edge] -> [ [Int] ]
vonKoch = map convert . solve
  where
    -- convert one solution to the correct format
    convert = map snd . IM.toAscList
