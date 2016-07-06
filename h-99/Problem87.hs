module Problem87 where

import qualified Problem85 as P85

import Graph
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Graph a = AdjForm a (Edge a)

mkGraph :: Ord a => [a] -> [(a,a)] -> Graph a
mkGraph vs es = fst (P85.mkGraph vs es)

{-
  gives adjacent vertices of a specified vertex in a graph.
  the resulting list should not have duplicates and
  is sorted guided by the Ord instance of vertex type
-}
adjacents :: Ord a => Graph a -> a -> [a]
adjacents (AdjForm g) v = maybe [] (sort . map getAdj . S.toList) (M.lookup v g)
  where
    getAdj (Edge a b) = if a == v then b else a

{-
  NOTE: in previous version of "search", I made a mistake:
  instead of traversing the whole graph, I only track one edge
  until it's not possible. a proper algorithm needs to backtrack
  in order to traversal nodes properly.
  the mistake is revealed by a simple check:
  because we aim to traverse all nodes, attention should be paid to
  any "ignore" pattern, as it potentially ignores the possibility of
  picking alternative nodes when a search have reached one dead end.
-}
search :: Ord a => Graph a -> [a] -> [a] -> [a]
search _ [] visited = reverse visited
search g (curV:todos) visited =
    search g newTodos newVisited
  where
    -- pick a list of not-yet visited adjacent vertices
    adjs = filter (`notElem` visited) (adjacents g curV)
    -- for depth-first search, we replace current node with
    -- a list of no-yet-visited adjacent nodes in the queue
    -- so that every node is exhaustively searched before moving to
    -- the next candidate of same depth.
    newTodos = removeDups $ adjs ++ todos
    newVisited = curV : visited

-- remove duplicated elements
removeDups :: Eq a => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = x : removeDups (filter (/= x) xs)

depthFirst :: Ord a => ([a], [(a,a)]) -> a -> [a]
depthFirst (vs,es) start = search (mkGraph vs es) [start] []
