{-# LANGUAGE ScopedTypeVariables #-}
module Problem83
  ( spantree
  ) where

import Graph
import qualified Data.Set as S
import Data.List

{-
  (INTERNAL ONLY)
  "search vsVisited es vsTodo" finds all possible spanning trees of a graph

  es: a list of edges where the spanning tree edges are picked from.
  vsVisited: the set of visited vertices
  vsTodo: the set of not-visited vertices

  INVARIANT: vsVisited and vsTodo should be disjoin and the union of them should
  be the full set of vertices of the graph.

-}
search :: Ord a => S.Set a -> [Edge a] -> S.Set a -> [ [Edge a] ]
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
                f (Edge a b) = a `S.notMember` vsVisited
                            || b `S.notMember` vsVisited
            -- (2) break "es'" into 2 parts:
            -- "newEsL" are edges whose one end is visited but not the other of it
            -- "newEsR" are edges whose both ends are not visited, which means
            -- we cannot add edges from "newEsR" immediately during this search iteration.
            (newEsL, newEsR) = partition isCandidate es'
              where
                isCandidate (Edge a b) =
                    ((a `S.member` vsVisited) && (b `S.notMember` vsVisited))
                 || ((b `S.member` vsVisited) && (a `S.notMember` vsVisited))
            -- "pick'" returns all choices of picking one element
            -- in addition to the element of choice, a list of remaining elements
            -- are also returned.
            -- e.g. pick' [1,2,3] = [ (1,[2,3]), (2,[3]), (3,[]) ]
            -- the name is chosen to resemble "pick".
            pick' = concatMap toPair . tails
              where
                toPair (a:as) = [(a,as)]
                toPair _ = []
        -- make one choice (nondeterministically)
        -- with "e" being the choice and "newEsL'" a list of remaining elements.
        (e@(Edge a b),newEsL') <- pick' newEsL
        let newEs = newEsL' ++ newEsR
            -- newV is the newly added vertex
            newV = if a `S.member` vsVisited then b else a
            newVsVisited = S.insert newV vsVisited
            newVsTodo = S.delete newV vsTodo
        resultEs <- search newVsVisited newEs newVsTodo
        pure (e:resultEs)

spantree :: Ord a => GraphForm a (Edge a) -> [ [Edge a] ]
spantree (GraphForm vs es) = search S.empty (S.toList es) vs
