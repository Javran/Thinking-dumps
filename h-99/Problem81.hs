{-# LANGUAGE MultiParamTypeClasses, TupleSections, FlexibleInstances, ConstraintKinds, FlexibleContexts #-}
module Problem81 where

import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Foldable

import Graph
import Problem80

toFndForm :: [a] -> [(a,a)] -> FndForm a (Edge a)
toFndForm vs es = FndForm $ map Left vs
                         ++ map (Right . uncurry Edge) es

findPaths :: Ord a => a -> a -> GraphForm a (Edge a) -> [ [a] ]
findPaths from to g = findPaths' [from] S.empty
    where
      (AdjForm gph) = graphFormToAdjForm g
      nextVertices v = foldMap (pairToList . terminals) . fromJust $ M.lookup v gph
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

-- undirected graph
main = print (paths 1 4 [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)])
