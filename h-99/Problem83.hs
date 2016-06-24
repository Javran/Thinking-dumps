{-# LANGUAGE ScopedTypeVariables #-}
module Problem83 where

import Graph
import Problem80
import qualified Data.Set as S
import Data.List

-- TODO:
-- * pick vs pick'

search :: S.Set Char -> [Edge Char] -> S.Set Char -> [ [Edge Char] ]
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
        let es' = filter f es
              where
                f (Edge a b) = a `S.notMember` vsVisited || b `S.notMember` vsVisited
            (newEsL, newEsR) = partition isCandidate es'
              where
                isCandidate (Edge a b) =
                    ((a `S.member` vsVisited) && (b `S.member` vsTodo))
                 || ((b `S.member` vsVisited) && (a `S.member` vsTodo))
            pick' = concatMap toPair . tails
              where
                toPair (a:as) = [(a,as)]
                toPair _ = []
        (e@(Edge a b),newEsL') <- pick' newEsL
        let newEs = newEsL' ++ newEsR
            newV = if a `S.member` vsVisited then b else a
            newVsVisited = S.insert newV vsVisited
            newVsTodo = S.delete newV vsTodo
        resultEs <- search newVsVisited newEs newVsTodo
        pure (e:resultEs)

spantree :: GraphForm Char (Edge Char) -> [ [Edge Char] ]
spantree (GraphForm vs es) = search S.empty (S.toList es) vs
