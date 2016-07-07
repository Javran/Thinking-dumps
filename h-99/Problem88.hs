module Problem88 where

import qualified Problem87 as P87
import qualified Data.Set as S

{-
  to find all connected components of a graph,
  we can keep track of not-yet-visited vertices,
  and pick one vertex from it as the starting point of "depthFirst".
  by doing so we can find one connected component from the graph.
  we then make this run through iterations until all vertices
  are visited (and belongs to one of the connected components)
-}
connectedComponents :: Ord a => ([a],[(a,a)]) -> [ [a] ]
connectedComponents p@(vs,_) = connectedComponents' initTodos
  where
    connectedComponents' todos = case S.minView todos of
        Nothing -> []
        Just (v,_) ->
              let result = P87.depthFirst p v
                  newVSet = S.fromList result
                  newTodos = todos `S.difference` newVSet
              in result : connectedComponents' newTodos
    initTodos = S.fromList vs
