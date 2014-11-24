{-# LANGUAGE MultiParamTypeClasses,TupleSections #-}
module Problem80 where

import Data.List
import Data.Either
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow

data GraphForm v e = GraphForm
    { gfVertices :: S.Set v
    , gfEdges    :: [e]
    }

data AdjForm v e = AdjForm (M.Map v [e])

data FndForm v e = FndForm [Either v e]

class VertexEdge v e where
    terminals :: e -> (v,v)
    terminalOf :: v -> e -> Bool

pairToList :: (a,a) -> [a]
pairToList ~(x,y) = [x,y]

graphFormToAdjForm :: (Ord v, VertexEdge v e) => GraphForm v e -> AdjForm v e
graphFormToAdjForm (GraphForm vs es) = AdjForm (M.fromListWith (++) pairs)
  where
    pairs = concatMap edgeToPair es
    edgeToPair e = map (,[e]) . pairToList . terminals $ e

adjFormToGraphForm :: (Eq e, Ord v, VertexEdge v e) => AdjForm v e -> GraphForm v e
adjFormToGraphForm (AdjForm as) = GraphForm vs es
  where
    vs = S.fromList . concatMap (pairToList . terminals) $ es
    es = nub . concat . M.elems $ as

fndFormToGraphForm :: (Ord v, VertexEdge v e) => FndForm v e -> GraphForm v e
fndFormToGraphForm (FndForm fs) = GraphForm vs es
  where
    vs = S.fromList . lefts $ fs
    es = rights fs

graphFormToFndForm :: GraphForm v e -> FndForm v e
graphFormToFndForm (GraphForm vs es) = FndForm (vs' ++ es')
  where
    vs' = map Left . S.toList $ vs
    es' = map Right es

main :: IO ()
main = _
