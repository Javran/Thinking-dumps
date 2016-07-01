module Problem86 where

import Graph
import Problem85

import qualified Data.Map.Strict as M
import qualified Data.Set as S

{-
  a. determine the degree of a given node.
-}
getDegree :: Ord a => a -> AdjForm a b -> Maybe Int
getDegree v (AdjForm g) = S.size <$> M.lookup v g

{-
  b. generate a list of all nodes sorted according to decreasing degree.
-}

degreeDecreasingNodes :: Ord a => AdjForm a b -> [a]
degreeDecreasingNodes = concatMap snd . reverse . degreeTable

{-
  TODO
  c. use Welsh-Powell's algorithm to paint the nodes of a graph
  in such a way that adjacent nodes have different colors.
-}
