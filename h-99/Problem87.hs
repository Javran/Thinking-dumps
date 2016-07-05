module Problem87 where

import qualified Problem85 as P85

import Graph
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Graph a = AdjForm a (Edge a)

mkGraph :: Ord a => [a] -> [(a,a)] -> Graph a
mkGraph vs es = fst (P85.mkGraph vs es)

adjacents :: Ord a => Graph a -> a -> [a]
adjacents (AdjForm g) v = maybe [] (sort . map getAdj . S.toList) (M.lookup v g)
  where
    getAdj (Edge a b) = if a == v then b else a

search :: Ord a => Graph a -> a -> S.Set a -> [a]
search g curV visited = case candidates of
    [] -> [curV]
    (next:_) ->
        let result = search g next (S.insert curV visited)
        in curV : result
  where
    candidates =
        filter (`S.notMember` visited)
      $ adjacents g curV

depthFirst :: Ord a => ([a], [(a,a)]) -> a -> [a]
depthFirst (vs,es) start = search (mkGraph vs es) start S.empty
