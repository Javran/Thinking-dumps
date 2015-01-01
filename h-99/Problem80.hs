{-# LANGUAGE MultiParamTypeClasses, TupleSections, FlexibleInstances, ConstraintKinds #-}
module Problem80 where

import Data.List hiding (concatMap, concat)
import Data.Either
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Monoid
import Data.Function

import Prelude hiding
    ( concat
    , concatMap
    )

import Graph

pairToList :: (a,a) -> [a]
pairToList ~(x,y) = [x,y]

graphFormToAdjForm :: OrdVE v e => GraphForm v e -> AdjForm v e
graphFormToAdjForm (GraphForm vs es) = AdjForm (M.fromListWith S.union allPairs)
  where
    allPairs = emptyPairs ++ vertEdgePairs
    -- used to ensure that isolated vertices are included
    emptyPairs = map (,S.empty) . S.toList $ vs
    vertEdgePairs = concatMap edgeToPair . S.toList $ es
    edgeToPair e
        | (a,b) <- terminals e = [(a, S.singleton e),(b, S.singleton e)]

adjFormToGraphForm :: OrdVE v e => AdjForm v e -> GraphForm v e
adjFormToGraphForm (AdjForm as) = GraphForm vs es
  where
    vs = M.keysSet as
    es = mconcat . M.elems $ as


fndFormToGraphForm :: OrdVE v e => FndForm v e -> GraphForm v e
fndFormToGraphForm (FndForm fs) = GraphForm vs (S.fromList es)
  where
    es = rights fs
    vs1 = lefts fs
    vs2 = concatMap (pairToList . terminals ) es
    vs = S.fromList $ vs1 ++ vs2

graphFormToFndForm :: OrdVE v e => GraphForm v e -> FndForm v e
graphFormToFndForm (GraphForm vs es) = FndForm (map Left vs' ++ map Right es')
  where
    -- vertices that appear at least once in the list of edges
    evs = S.fromList . concatMap (pairToList . terminals) $ es
    vs' = S.toList $ evs `S.union` vs
    es' = S.toList es

example :: GraphForm Char (Edge Char)
example = GraphForm vs es
  where
    vs = S.fromList "ghbcfkd"
    es = S.fromList (map (uncurry Edge)
                         [ ('g','h')
                         , ('b','c')
                         , ('b','f')
                         , ('f','c')
                         , ('f','k')
                         ])

main :: IO ()
main = do
    print example
    print (graphFormToAdjForm example)
    print (graphFormToFndForm example)
