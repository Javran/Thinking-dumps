{-# LANGUAGE ScopedTypeVariables #-}
module Problem83 where

import Graph
import Problem80
import qualified Data.Set as S
import Data.List

-- "pick" from Problem90
-- TODO:
-- - implement pick without using much lib functions
--   (as I feel there might be some performance gain
-- - export as an util function
pick :: forall a. [a] -> [(a,[a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split :: ([a], [a]) -> (a,[a])
    split (ls,v:rs) = (v,ls++rs)
    split _ = error "cannot split empty list"

-- TODO: incomplete

-- see: http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
--- http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p83.gif

k4RawGraph :: String
-- TODO: (we need to change a name for this)
-- k4RawGraph = "ab bc ce eh hg gf fd da de be dg"
-- "k4" in the problem actually means the following graph:
k4RawGraph = "ab bc cd da ac bd"

k4Edges :: [Edge Char]
k4Edges = map parseEdge . words $ k4RawGraph
  where
    parseEdge (a:b:_) = Edge a b
    parseEdge _ = error "bad list"

k4Vertices :: S.Set Char
k4Vertices = S.fromList (concatMap (\(Edge a b) -> [a,b]) k4Edges)

-- TODO:
-- * pick vs pick'

search :: S.Set Char -> [Edge Char] -> S.Set Char -> [ [Edge Char] ]
search vsVisited es vsTodo
    | S.null vsTodo = pure []
    | S.null vsVisited =
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

test = search S.empty k4Edges k4Vertices
