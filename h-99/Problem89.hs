module Problem89 where

{-
  core idea taken from:
  https://en.wikipedia.org/wiki/Bipartite_graph#Testing_bipartiteness

  - perform depth search and color nodes, during which time check for consistency.
  - note that it might be cases where the graph is not a single connected component,
    and in that case we need to check if every connected component.
-}

import Graph
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Function
import Control.Monad
import Data.Maybe

import Problem87 (Graph, mkGraph, removeDups)

-- like Problem87.adjacents but in this problem there's
-- no need for sorting nodes.
adjacents :: Ord a => Graph a -> a -> [a]
adjacents (AdjForm g) v = maybe [] (map getAdj . S.toList) (M.lookup v g)
  where
    getAdj (Edge a b) = if a == v then b else a

checkBipartite :: (Ord a, Show a) => Graph a -> [Edge a] -> S.Set a -> [a] -> M.Map a Bool -> Maybe ()
checkBipartite _ [] _ _ _ = pure ()
checkBipartite g es vSet [] _ = case S.minView vSet of
    Just (v,vSet') ->
        -- dropping old colorMap because this is got to be a new connected component
        -- so there's no need to keep old one around.
        checkBipartite g es vSet' [v] M.empty
    Nothing -> guard (null es)
checkBipartite g es vSet (curV:todos) colorMap = do
    -- colorMap1: to make sure curV is colored. as "todos" is expanded from taking
    -- its first element and put back its adjacent nodes, the only case where "curV"
    -- is not yet assigned a color is that when we start searching on a new connected
    -- component of a graph.
    let (colorMap1,curColor) = case M.lookup curV colorMap of
            Nothing -> (M.insert curV False colorMap, False)
            Just c -> (colorMap, c)
        -- Bools are used for coloring, inversing a color results in getting the other color.
        invColor = not curColor
        adjs = adjacents g curV
    -- first pass to color adjacent nodes of curV (with early failure)
    colorMap2 <- fix (\loop curAdjs curColorMap -> case curAdjs of
                          [] ->
                              -- all adjacent nodes have been checked and colored
                              pure curColorMap
                          (v1:vs1) -> case M.lookup v1 curColorMap of
                              Nothing ->
                                  -- this adjacent node has not yet assigned a color
                                  loop vs1 (M.insert v1 invColor curColorMap)
                              Just c ->
                                  -- an adjacent node that has been assigned a color
                                  -- in this case it's required to make sure that
                                  -- its color is the intended one.
                                  guard (c == invColor) >> loop vs1 curColorMap
                     ) adjs colorMap1
    -- second pass to check remaining edges.
    remainedEs <- fix (\loop curEs remainedEs -> case curEs of
                           [] ->
                               -- all edges have been checked, return the list of
                               -- not-yet-checkable edges.
                               pure remainedEs
                           (Edge a b:curEs')
                               | Just va <- M.lookup a colorMap2
                               , Just vb <- M.lookup b colorMap2
                                 -- when both ends are assigned a color
                                 -> guard (va /= vb) >> loop curEs' remainedEs
                           (e:curEs') ->
                               -- if the pattern matching has failed,
                               -- then one of (or both) ends are not yet assigned
                               -- a color thus not yet checkable, we keep it in the pending list
                               loop curEs' (e:remainedEs)
                       ) es []
    -- vSet should hold elements not-yet visited.
    -- and visited vertices should be removed from the queue
    let newVSet = S.delete curV vSet
        newTodos = filter (`S.member` newVSet) $ removeDups $ adjs ++ todos
    checkBipartite g remainedEs newVSet newTodos colorMap2

bipartite :: (Ord a, Show a) => ([a], [(a,a)]) -> Bool
bipartite (vs, rawEs) = isJust (checkBipartite g es vSet [] M.empty)
  where
    g = mkGraph vs rawEs
    es = map (uncurry Edge) rawEs
    vSet = S.fromList vs
