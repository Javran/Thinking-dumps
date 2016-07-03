{-# LANGUAGE ScopedTypeVariables #-}
module Problem86 where

import Graph
import Problem85

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe

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
  c. use Welsh-Powell's algorithm to paint the nodes of a graph
  in such a way that adjacent nodes have different colors.

  Related links:
  - http://graphstream-project.org/doc/Algorithms/Welsh-Powell/
  - https://en.wikipedia.org/wiki/Graph_coloring#Greedy_coloring

  NOTE:
  - according to the algorithm, we can take iterations to pick one color and traverse
    the vertex list, coloring nodes that haven't been colored and don't have
    any adjacent vertex that has that color.
-}

{-
  "colorNodes af colorMap color xs" traverses node list "xs" in order,
  and use "color" to color nodes that are not color and don't have any adjacent nodes
  with that color.
  "colorMap" is the existing node-color mapping and "colorNodes" will extend it with the new
  color. in addition, nodes not qualified for coloring are also returned in a list
  with node order preserved.
-}
colorNodes :: forall a color. (Ord a, Enum color, Eq color) =>
              AdjForm a (Edge a) -> M.Map a color -> color -> [a] -> ([a], M.Map a color)
colorNodes _ colorMap _ [] = ([],colorMap)
colorNodes af@(AdjForm g) colorMap curColor (x:xs) =
    if noConflict
      then colorNodes af (M.insert x curColor colorMap) curColor xs
      else let (xs',newColorMap) = colorNodes af colorMap curColor xs
           in (x:xs',newColorMap)
  where
    neighbors :: [a]
    neighbors = map get . S.toList . fromJust $ M.lookup x g
      where
        -- get the end other than "x" of this edge
        get (Edge a b) = if a == x then b else a
    noConflict = all (check . (`M.lookup` colorMap)) neighbors
      where
        check = maybe True (/= curColor)

graphColoring :: forall color a . (Enum color, Eq color, Ord a) => AdjForm a (Edge a) -> color -> [a] -> M.Map a color
graphColoring g initC = graphColoring' initC M.empty
  where
    graphColoring' c colorMap xs = case remained of
        [] -> colorMap'
        _ -> graphColoring' (succ c) colorMap' remained
      where
        (remained, colorMap') = colorNodes g colorMap c xs
