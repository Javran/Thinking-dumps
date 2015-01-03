{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
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

findPaths :: Ord a => a -> a -> GraphForm a (Arc a) -> [ [a] ]
findPaths from to g = findPaths' [from] S.empty
    where
      (AdjForm gph) = graphFormToAdjForm g
      -- be extreme careful here that the adjacent form gets *all the edges*
      -- related to a certain vertex in its map,
      -- in order to extract vertices that it points to (excluding those
      -- that points to it) we need "extraceDstFrom" function
      extractDstFrom vFrom (v1,v2) = [ v2 | v1 == vFrom ]
      nextVertices v = foldMap (extractDstFrom v . terminals) . fromJust $ M.lookup v gph
      findPaths' candidates visited = do
          next <- candidates
          guard $ next `S.notMember` visited
          if next == to
             then return [to]
             else do
               restPath <- findPaths' (nextVertices next) (S.insert next visited)
               return (next:restPath)

paths :: Ord a => a -> a -> [(a,a)] -> [ [a] ]
paths from to g = findPaths from to ( fndFormToGraphForm
                                    . toFndForm [] $ g)

main :: IO ()
main = do
    print (paths (1::Int) 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
    print (paths (2::Int) 6 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
