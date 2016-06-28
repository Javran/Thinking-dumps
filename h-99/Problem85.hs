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
import Control.Arrow

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

-- for 2 graphs to be isomorphic, the degree table must match
-- returns more properly structured data when succeeded: [(Int, ([a],[b]))]
-- in which first Int is still the degree, and ([a],[b]) are 2 lists of the same size.
-- if 2 graphs are indeed isomorphic, then vertices' mapping must be established inside
-- of every group.
-- note that it's not necessary for vertices of 2 graphs to have the same type
-- as long as the mapping can be established (which means at least "Ord" needs to
-- be supported by the corresponding type), everything should work.
checkDegreeTables :: [(Int,[a])] -> [(Int,[b])] -> Maybe [(Int, ([a],[b]))]
checkDegreeTables dt1 dt2
    | convert dt1 == convert dt2 = Just (zip (map fst dt1)
                                             (zip (map snd dt1)
                                                  (map snd dt2)))
    | otherwise = Nothing
  where
    convert = map (second length)
