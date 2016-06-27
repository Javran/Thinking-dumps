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

mkGraph :: Ord a => [a] -> [(a,a)] -> AdjForm a (Edge a)
mkGraph vs es = graphFormToAdjForm (GraphForm vSet eSet)
  where
    vSet = S.fromList vs
    eSet = S.fromList . map (uncurry Edge) $ es
