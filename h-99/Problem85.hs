{-# LANGUAGE ScopedTypeVariables #-}
module Problem85 where

{-

  plan:
  - search by purposing partial bijections, reject immediately
    if the iso cannot be established in the given way
  - group nodes by their degrees, by which we can reduce search space.

-}


import Graph
import Problem80

import qualified Data.Set as S
import qualified Data.Map.Strict as M

mkGraph :: Ord a => [a] -> [(a,a)] -> AdjForm a (Edge a)
mkGraph vs es = graphFormToAdjForm (GraphForm vSet eSet)
  where
    vSet = S.fromList vs
    eSet = S.fromList . map (uncurry Edge) $ es

degreeTable :: forall a b. Ord a => AdjForm a b -> [(Int,[a])]
degreeTable (AdjForm g) =
        M.toAscList
      . M.map S.toList
      . M.fromListWith mappend
      . map (\(v,d) -> (d,S.singleton v))
      $ degrees
  where
    -- the degree of a vertex is the number of edges connected to it.
    -- (undirected)
    degrees :: [(a, Int)]
    degrees = M.toList $ M.map S.size g
