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
colorNodes (AdjForm g) cm curColor = colorNodes' cm
  where
    colorNodes' colorMap [] = ([], colorMap)
    colorNodes' colorMap (x:xs) =
        if noConflict
          then
            -- assigning color to node "x"
            colorNodes' (M.insert x curColor colorMap) xs
          else
            -- "x" does not meet the requirement,
            -- so when rest of the task is done we put it back.
            let (xs',newColorMap) = colorNodes' colorMap xs
            in (x:xs',newColorMap)
      where
        neighbors :: [a]
        neighbors = map get . S.toList . fromJust $ M.lookup x g
          where
            -- get the end other than "x" of this edge
            get (Edge a b) = if a == x then b else a
        -- check all neighborhoods of "x"
        noConflict = all (check . (`M.lookup` colorMap)) neighbors
          where
            -- for each neighboring node, it should either not yet assigned a color
            -- or assigned a color different than the current one.
            check = maybe True (/= curColor)

{-
  "graphColoring g c" takes a graph "g" and initial color "c" to color the graph.
  successors of "c" is used when necessary (to resolve color-conflict)
-}
graphColoring :: forall color a.
                 (Enum color, Eq color, Ord a) =>
                 AdjForm a (Edge a) -> color -> [a] -> M.Map a color
graphColoring g initC = graphColoring' initC M.empty
  where
    -- loop until there is no remained nodes
    -- this process will eventually terminate as long as node list is finite,
    -- because each time we will take a "fresh" color (by using "succ")
    -- so there's at least one node colored during every iteration.
    graphColoring' c colorMap xs = case remained of
        [] -> colorMap'
        _ -> graphColoring' (succ c) colorMap' remained
      where
        (remained, colorMap') = colorNodes g colorMap c xs

kcolor :: Ord a => [a] -> [(a,a)] -> [(a,Int)]
kcolor vs es = M.toAscList $ graphColoring g 1 vsSorted
  where
    (g, _) = mkGraph vs es
    vsSorted = degreeDecreasingNodes g

{-
 TODO: tests
  working example:

let g = fst $ mkGraph ['a','b','c','d','e','f','g','h','i','j'] [('a','b'),('a','e'),('a','f'),('b','c'),('b','g'),('c','d'),('c','h'),('d','e'),('d','i'),('e','j'),('f','h'),('f','i'),('g','i'),('g','j'),('h','j')]
let ns = degreeDecreasingNodes g
graphColoring g (1 :: Int) ns

-}
