{-# LANGUAGE ScopedTypeVariables #-}
module Problem92 where

import Data.List
import Data.Maybe
import Control.Monad

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Utils

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Use uncurry" #-}

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

{-
  summary of arguments
  * t: the tree
  * missingEdges: a list of edges that have not yet taken into account
  * missingDiffs: a set of Ints that has not yet observed as an "edge difference"
  * assigned: current (partial) assignment of numbers to nodes
  * remainings: not-yet-assigned numbers
  * todo: nodes not visited

  our target is to assigned different numbers to nodes to satisfy the condition.
  for search algorithms in general, "t" is the graph info we can refer to, and "assigned"
  make it possible to construct full solutions (if any)
  and "todo" serves as a queue of not yet considered nodes.

  several things are kept in order to reduce the amount of duplicated work:
  * missingEdges: only those whose both ends are assigned with a number are picked and checked,
    as more and more nodes are assigned, this collection will eventually become empty
  * missingDiffs: to save us from checking whether all current differences are unique over and over
    again, we take another view on the problem: since all unique differences
    has to be assigned to an edge, we just need to test whether all newly added edges
    has unique differences.
-}
search :: Tree -> [Edge] -> IS.IntSet -> NodeAssigns -> [Int] -> [Int] -> [NodeAssigns]
search t missingEdges missingDiffs assigned remainings todo = case todo of
    [] -> do
      -- all numbers are assigned, check whether we have missed something
      guard (null missingEdges && IS.null missingDiffs)
      -- if not, this should be a solution
      pure assigned
    (cur:newTodo) -> do
      -- pick one number
      (newNum, newRemainings) <- pick remainings
      -- make new assignment: number "newNum" to node "cur"
      let newAssigned = IM.insert cur newNum assigned
          isRelated (a,b)
              | a == cur = isJust (IM.lookup b newAssigned)
              | b == cur = isJust (IM.lookup a newAssigned)
              | otherwise = False
          find' node = fromJust $ IM.lookup node newAssigned
          getDiff (x,y) = abs (subtract (find' x) (find' y))
          -- partition missing edges into those that has both end assigned
          -- and those don't (and the latter remains in the list)
          (relatedEdges, newMissingEdges) = partition isRelated missingEdges
          -- the set of newly added differences
          newDiffs = IS.fromList $ map getDiff relatedEdges
          -- the set of differences (merging existing and new ones)
          newMergedDiffs = missingDiffs `IS.union` newDiffs
      -- there should not be any overlap about differences
      guard $ IS.null $ missingDiffs `IS.intersection` newDiffs
      search t newMissingEdges newMergedDiffs newAssigned newRemainings newTodo

solve :: [Edge] -> [NodeAssigns]
solve es = search t es (IS.fromList [1..l-1]) IM.empty nodes nodes
  where
    l = length es + 1
    nodes = [1..l]
    t = foldl' (flip addEdge) IM.empty es

vonKoch :: [Edge] -> [ [Int] ]
vonKoch = map convert . solve
  where
    -- convert one solution to the correct format
    convert = map snd . IM.toAscList
