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

search t edges assigned remainings todo cur
    | IS.null todo = pure assigned
    | otherwise = do
        let newTodo = IS.delete cur todo
        (newNum, newRemainings) <- pick remainings
        let newAssigned = IM.insert cur newNum assigned
            validEdges = filter (\(x,y) -> IM.member x assigned && IM.member y assigned) edges
            find' k m = fromJust $ IM.lookup k m
            edgeDiffs = map (\(x,y) -> abs (find' x assigned - find' y assigned)) validEdges
        guard $ uniqueInts edgeDiffs
        search t edges newAssigned newRemainings newTodo (fst . fromJust $ IS.minView newTodo)
