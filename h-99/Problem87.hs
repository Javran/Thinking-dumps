module Problem87 where

import qualified Problem85 as P85

import Graph
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Graph a = AdjForm a (Edge a)

mkGraph :: Ord a => [a] -> [(a,a)] -> Graph a
mkGraph vs es = fst (P85.mkGraph vs es)

adjacents :: Ord a => Graph a -> a -> [a]
adjacents (AdjForm g) v = maybe [] (map getAdj . S.toList) (M.lookup v g)
  where
    getAdj (Edge a b) = if a == v then b else a
