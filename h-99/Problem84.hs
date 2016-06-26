{-# LANGUAGE ScopedTypeVariables #-}
module Problem84
  ( prim
  ) where

import qualified Data.Set as S
import Data.Ord
import Data.List
import Data.Maybe
import Control.Monad

import Graph

type WeightedEdge a w = (Edge a, w)

{-
  NOTE: it's totally possible to implement "search" so that
  all solutions are returned as a list: the idea is to collect a list
  of smallest weighted edges (could be many, which results in many minimum spanning trees)
  so we can choose non-determistically among them.

  we choose not to do so because to do it in an efficient manner (without unnecessary list
  traversal) requires something like "list'" in Problem83.hs, and that complicates
  the code but adding nothing significant in terms of problem solving.
  so for now I'm good with returning at most one solution.

-}

-- like "search" in Problem83. only modified part will be explained
search :: forall a w. (Ord a, Ord w) =>
          S.Set a -> [WeightedEdge a w] -> S.Set a ->
          Maybe [WeightedEdge a w]
search vsVisited es vsTodo
    | S.null vsTodo =
        pure []
    | S.null vsVisited =
        let Just (v,rest) = S.minView vsTodo
        in search (S.singleton v) es rest
    | otherwise = do
        let es' = filter f es
              where
                f (Edge a b, _) = a `S.notMember` vsVisited
                               || b `S.notMember` vsVisited
            (newEsL, newEsR) = partition isCandidate es'
              where
                isCandidate (Edge a b, _) =
                    ((a `S.member` vsVisited) && (b `S.notMember` vsVisited))
                 || ((b `S.member` vsVisited) && (a `S.notMember` vsVisited))
            -- sort edges by their weights, and we need to pick one edge with
            -- smallest weight for the minimum spanning tree.
            newEsLSorted = sortBy (comparing snd) newEsL
        -- check that the candidate list is non-empty
        -- so after this point we can safely assume that "newEsLSorted" is not empty
        guard $ not (null newEsLSorted)
        let (e@(Edge a b, _):newEsL') = newEsLSorted
            newEs = newEsL' ++ newEsR
            newV = if a `S.member` vsVisited then b else a
            newVsVisited = S.insert newV vsVisited
            newVsTodo = S.delete newV vsTodo
        resultEs <- search newVsVisited newEs newVsTodo
        pure (e:resultEs)

prim :: (Ord a, Ord w) => [a] -> [(a,a,w)] -> [(a,a,w)]
prim vs es = fromMaybe [] (map convert <$> search S.empty es' (S.fromList vs))
  where
    es' = map (\(a,b,w) -> (Edge a b, w)) es
    convert (Edge a b, w) = (a,b,w)
