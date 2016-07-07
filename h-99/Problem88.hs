module Problem88 where

import qualified Problem87 as P87
import qualified Data.Set as S

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
