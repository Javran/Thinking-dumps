{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Problem82 where

import Control.Monad
import Control.Applicative
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Foldable

import Graph
import Problem80
import Problem81 (Arc(..),toFndForm)

-- TODO: we can have a general "nextVertices"
-- and have it implemented correct once for all.

findCycles :: forall a . Ord a => a -> GraphForm a (Arc a) -> [ [a] ]
findCycles from g = (from:) <$> findCycles' initNexts (S.singleton from)
  where
    initNexts = nextVertices from
    (AdjForm gph) = graphFormToAdjForm g
    -- be extreme careful here that the adjacent form gets *all the edges*
    -- related to a certain vertex in its map,
    -- in order to extract vertices that it points to (excluding those
    -- that points to it) we need "extraceDstFrom" function
    extractDstFrom vFrom (v1,v2) = [ v2 | v1 == vFrom ]
    nextVertices v = foldMap (extractDstFrom v . terminals)
                   . fromJust
                   $ M.lookup v gph
    findCycles' :: Ord a => [a] -> S.Set a -> [ [a] ]
    findCycles' candidates visited = do
        next <- candidates
        if from == next
           then return [next]
           else do
             guard $ next `S.notMember` visited
             restCycle <- findCycles' (nextVertices next) (S.insert next visited)
             return (next:restCycle)

cycles :: Ord a => a -> [ (a,a) ] -> [ [a] ]
cycles from g = findCycles from (fndFormToGraphForm . toFndForm [] $ g)

main :: IO ()
main = do
    print $ cycles (2 :: Int) [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print $ cycles (1 :: Int) [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
