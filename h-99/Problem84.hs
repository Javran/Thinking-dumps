{-# LANGUAGE ScopedTypeVariables #-}
module Problem84 where

import qualified Data.Set as S
import Data.Ord
import Data.List
import Data.Maybe
import Graph

type WeightedEdge a w = (Edge a, w)

-- TODO: impl
-- TODO: return multiple solutions?
search :: forall a w. (Ord a, Ord w) =>
          S.Set a -> [WeightedEdge a w] -> S.Set a -> Maybe [WeightedEdge a w]
search vsVisited es vsTodo
    | S.null vsTodo =
        -- trivial solution, all vertices are covered
        pure []
    | S.null vsVisited =
        -- at the beginning of this search, the caller should provide
        -- an empty "vsVisited". but without a visited node, the searching
        -- process can't be initiated, so here we randomly choose one node from "vsVisited"
        -- to start the search.
        -- (this set won't be empty otherwise the first case should capture it.)
        -- by "randomly" I mean any element of "vsVisited" will do, both "minView" and "maxView"
        -- should work fine.
        let Just (v,rest) = S.minView vsTodo
        in search (S.singleton v) es rest
    | otherwise = do
        let -- (1) first removed some edges from candidate list:
            -- if both ends of an edge are included in "vsVisited",
            -- adding this edge will result in a loop.
            -- so there is no need to consider these edges.
            es' = filter f es
              where
                f (Edge a b, _) = a `S.notMember` vsVisited
                               || b `S.notMember` vsVisited
            -- (2) break "es'" into 2 parts:
            -- "newEsL" are edges whose one end is visited but not the other of it
            -- "newEsR" are edges whose both ends are not visited, which means
            -- we cannot add edges from "newEsR" immediately during this search iteration.
            (newEsL, newEsR) = partition isCandidate es'
              where
                isCandidate (Edge a b, _) =
                    ((a `S.member` vsVisited) && (b `S.notMember` vsVisited))
                 || ((b `S.member` vsVisited) && (a `S.notMember` vsVisited))

            newEsLSorted = sortBy (comparing snd) newEsL
        -- make one choice (nondeterministically)
        -- with "e" being the choice and "newEsL'" a list of remaining elements.
        case newEsLSorted of
          [] -> Nothing
          (e@(Edge a b, _):newEsL') -> do
            let newEs = newEsL' ++ newEsR
                -- newV is the newly added vertex
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
