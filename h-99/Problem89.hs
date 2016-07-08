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
import Data.List
import Data.Maybe

import Problem87 (Graph, mkGraph, removeDups)

-- like Problem87.adjacents but in this problem there's
-- no need for sorting nodes.
adjacents :: Ord a => Graph a -> a -> [a]
adjacents (AdjForm g) v = maybe [] (map getAdj . S.toList) (M.lookup v g)
  where
    getAdj (Edge a b) = if a == v then b else a

checkBipartite :: Ord a => Graph a -> [Edge a] -> S.Set a -> [a] -> M.Map a Bool -> Maybe ()
checkBipartite _ [] _ _ _ = pure ()
checkBipartite g es vSet [] _ = case S.minView vSet of
    Just (v,vSet') -> checkBipartite g es vSet' [v] M.empty
    Nothing -> pure ()
checkBipartite g es vSet (curV:todos) colorMap = do
    let (colorMap1,curColor) = case M.lookup curV colorMap of
            Nothing -> (M.insert curV False colorMap, False)
            Just c -> (colorMap, c)
        invColor = not curColor
        adjs = adjacents g curV
    -- first pass to color adjacent nodes of curV (with early failure)
    colorMap2 <- fix (\loop curAdjs curColorMap -> case curAdjs of
                          [] -> pure curColorMap
                          (v1:vs1) -> case M.lookup v1 curColorMap of
                              Nothing -> loop vs1 (M.insert v1 invColor curColorMap)
                              Just c -> guard (c == invColor) >> loop vs1 curColorMap
                     ) adjs colorMap1
    remainedEs <- fix (\loop curEs remainedEs -> case curEs of
                           [] -> pure remainedEs
                           (Edge a b:curEs')
                               | Just va <- M.lookup a colorMap2
                               , Just vb <- M.lookup b colorMap2
                                 -> guard (va /= vb) >> loop curEs' remainedEs
                           (e:curEs') -> loop curEs' (e:remainedEs)
                       ) es []
    let newVSet = foldl' (flip S.delete) vSet (curV:adjs)
        newTodos = removeDups $ adjs ++ todos
    checkBipartite g remainedEs newVSet newTodos colorMap2

bipartite :: Ord a => ([a], [(a,a)]) -> Bool
bipartite (vs, rawEs) = isJust (checkBipartite g es vSet [] M.empty)
  where
    g = mkGraph vs rawEs
    es = map (uncurry Edge) rawEs
    vSet = S.fromList vs
