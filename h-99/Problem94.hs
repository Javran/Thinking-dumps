{-# LANGUAGE ScopedTypeVariables #-}
module Problem94 where

import Data.List
import Graph
import Problem80
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Map.Strict as M

import qualified Problem85 as P85
import Problem85 (Graph)

-- pick one element from the list with context (remained elements).
-- unlike "Utils.pick", "pickOne" will cut all elements
-- in front of the chosen element.
pickOne :: [a] -> [(a,[a])]
pickOne = map (\(x:xs) -> (x,xs)) . init . tails

-- pick n elements from a list with context (remained elements).
-- the resulting sequence of the chosen elements is always a subsequence
-- of the original input.
-- e.g. it's possible for "pickN _ [1..10]" to return "[1,4,5,6..]"
-- as one of its result, but returning "[1,4,6,5]" is not possible
-- because there's no way for "6,5" to be a proper subsequence of "[1..10]"
pickN :: Int -> [a] -> [([a],[a])]
pickN 0 xs = [([],xs)]
pickN n xs = do
    (y,ys) <- pickOne xs
    (z,zs) <- pickN (n-1) ys
    pure (y:z,zs)

{-
  (internal use only)
  "genGraph k vs gragh" generates k-regular graphs. where "vs" is the complete
  list of vertices and each vertex should have its corresponding entity in "graph"
  (meaning that looking up that vertex won't fail)

  INVARIANT: at the beginning of every call to this function, "vs" should only
  contain vertices whose doesn't have sufficient degrees. so that when the list
  becomes empty, we automatically know the graph generation is complete.
-}
genGraph :: forall a. Ord a => Int -> [a] -> Graph a -> [Graph a]
genGraph _ [] graph = pure graph
genGraph k (v:vs) g = do
    -- figure out how many vertices we need to connect to "v"
    let curDeg = S.size $ fromJust $ M.lookup v (getGraph g)
    -- nondeterministically picking up the right number of nodes
    (newAdjs,_) <- pickN (k-curDeg) vs
    -- and create new edges to connect "v" and all "newAdjs"
    let g1 = foldl' (flip (addEdge v)) g newAdjs
        -- cleanup "vs", note that we don't have to re-calculate degrees
        -- of all pending vertices. only those we have just updated.
        toBeRemoved =
          v : filter (\adj -> k == (S.size . fromJust $ M.lookup adj (getGraph g1))) newAdjs
        newVs = foldl' (flip delete) vs toBeRemoved
    genGraph k newVs g1
  where
    getGraph (AdjForm graph,_) = graph
    addEdge :: a -> a -> Graph a -> Graph a
    addEdge a b (AdjForm m,es) =
        (AdjForm . M.adjust (S.insert e) a . M.adjust (S.insert e) b $ m, e:es)
      where
        e = Edge a b

convertGraph :: Ord a => AdjForm a (Edge a) -> P85.Graph a
convertGraph af@(AdjForm g) = (af, S.toList $ S.fromList $ concatMap (S.toList . snd) (M.toList g))

{-# ANN insertIso "HLint: ignore Avoid lambda" #-}
insertIso :: Ord a => Graph a -> [Graph a] -> [Graph a]
insertIso g xs = if all (\x -> not $ P85.bigIso g x) xs then g:xs else xs

removeIsos :: Ord a => [Graph a] -> [Graph a]
removeIsos = foldl' (flip insertIso) []

regular :: Int -> Int -> [Graph Int]
regular n k = removeIsos $ genGraph k [1..n] g2
  where
    g1 = fndFormToGraphForm (FndForm (map Left [1..n]))
    g2 = (graphFormToAdjForm g1, [])
