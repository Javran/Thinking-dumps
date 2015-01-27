{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, ScopedTypeVariables #-}
module Problem82 where

import Control.Monad
import Control.Applicative
import qualified Data.Set as S

import Graph
import Problem80
import Problem81 (Arc(..),toFndForm,nextVertices)

findCycles :: forall a . Ord a => a -> GraphForm a (Arc a) -> [ [a] ]
findCycles from g = (from:) <$> findCycles' initNexts (S.singleton from)
  where
    gph = graphFormToAdjForm g
    initNexts = nextVertices gph from
    findCycles' :: Ord a => [a] -> S.Set a -> [ [a] ]
    findCycles' candidates visited = do
        next <- candidates
        if from == next
           then return [next]
           else do
             guard $ next `S.notMember` visited
             restCycle <- findCycles' (nextVertices gph next) (S.insert next visited)
             return (next:restCycle)

cycles :: Ord a => a -> [ (a,a) ] -> [ [a] ]
cycles from g = findCycles from (fndFormToGraphForm . toFndForm [] $ g)

main :: IO ()
main = do
    print $ cycles (2 :: Int) [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
    print $ cycles (1 :: Int) [(1,2),(2,3),(1,3),(3,4),(4,2),(5,6)]
