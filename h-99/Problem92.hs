{-# LANGUAGE ScopedTypeVariables #-}
module Problem92 where

import Data.List
import Data.Maybe
import Data.Function
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

addEdge :: (Int, Int) -> Tree -> Tree
addEdge (a,b) = insertEdge a b . insertEdge b a
  where
    insertEdge x y = IM.alter (Just . IS.insert x . fromMaybe IS.empty) y

-- all undirected edges
allUndirEdges :: Tree -> [(Int,Int)]
allUndirEdges t = filter (\(x,y) -> x < y) edges
  where
    edges = do
      (k,vs) <- IM.toList t
      v <- IS.toList vs
      pure (k,v)

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
search :: Tree -> [(Int,Int)] -> NodeAssigns -> [Int] -> IS.IntSet -> Int -> [NodeAssigns]
search t edges assigned remainings todo cur
    | IS.null todo = do
        let edgeDiffs = map (\(x,y) -> abs $ (subtract `on` find' assigned) x y) edges
        guard $ and $ zipWith (==) (sort edgeDiffs) [1..]
        pure assigned
    | otherwise = do
        let newTodo = IS.delete cur todo
        (newNum, newRemainings) <- pick remainings
        let newAssigned = IM.insert cur newNum assigned
            validEdges = filter (\(x,y) -> IM.member x newAssigned && IM.member y newAssigned) edges
            edgeDiffs = map (\(x,y) -> abs $ (subtract `on` find' newAssigned) x y) validEdges
        guard $ uniqueInts edgeDiffs
        search t edges newAssigned newRemainings newTodo (fst . fromJust $ IS.minView newTodo)
  where
    find' m k = fromJust $ IM.lookup k m

solve :: [(Int,Int)] -> [NodeAssigns]
solve es = search t (allUndirEdges t) IM.empty [1..l] (IS.fromList [1..l]) 1
  where
    l = length es + 1
    t = foldl' (flip addEdge) IM.empty es
