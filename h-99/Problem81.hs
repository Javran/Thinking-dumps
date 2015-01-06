{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ConstraintKinds #-}
module Problem81 where

import Control.Monad
import Data.Foldable
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Graph
import Problem80

data Arc a = Arc a a deriving (Show,Ord,Eq)

instance Eq v => VertexEdge v (Arc v) where
    terminals (Arc a b) = (a,b)

toFndForm :: [a] -> [(a,a)] -> FndForm a (Arc a)
toFndForm vs es = FndForm $ map Left vs
                         ++ map (Right . uncurry Arc) es

-- | given a graph in its AdjForm and a vertex,
--   returns a list of vertices that this vertex points to
nextVertices :: OrdVE v e => AdjForm v e -> v -> [v]
-- be extreme careful here that the adjacent form gets *all the edges*
-- related to a certain vertex in its map,
-- this means the edge list will include those that points to that vertex.
-- but we are lucky here, since "terminals" will tell us its source
-- and destination, so we won't get mess up
nextVertices (AdjForm g) vFrom = case M.lookup vFrom g of
    Nothing -> []
    Just es -> foldMap (extractDst . terminals) es
  where
    extractDst (v1,v2) = [v2 | v1 == vFrom ]

findPaths :: Ord a => a -> a -> GraphForm a (Arc a) -> [ [a] ]
findPaths from to g = findPaths' [from] S.empty
    where
      gph = graphFormToAdjForm g
      findPaths' candidates visited = do
          next <- candidates
          guard $ next `S.notMember` visited
          if next == to
             then return [to]
             else do
               restPath <- findPaths' (nextVertices gph next) (S.insert next visited)
               return (next:restPath)

paths :: Ord a => a -> a -> [(a,a)] -> [ [a] ]
paths from to g = findPaths from to ( fndFormToGraphForm
                                    . toFndForm [] $ g)

main :: IO ()
main = do
    print (paths (1::Int) 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
    print (paths (2::Int) 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
