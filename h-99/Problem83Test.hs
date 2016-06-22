module Problem83Test where


import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import Graph
import Problem83

{-# ANN module "HLint: ignore Redundant do" #-}

-- "g1___" stands for the example graph referred to as "k4" in:
-- https://wiki.haskell.org/99_questions/Solutions/83
g1Raw :: String
-- TODO: (we need to change a name for this)

-- "k4" in the problem actually means the following graph:
g1Raw = "ab bc cd da ac bd"

g1Edges :: [Edge Char]
g1Edges = map parseEdge . words $ g1Raw
  where
    parseEdge (a:b:_) = Edge a b
    parseEdge _ = error "bad list"

g1Vertices :: S.Set Char
g1Vertices = S.fromList (concatMap (\(Edge a b) -> [a,b]) g1Edges)

-- see: http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
--- http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p83.gif
g2Raw :: String
g2Raw = "ab bc ce eh hg gf fd da de be dg"

test = search S.empty g1Edges g1Vertices

main = hspec $ do
    describe "random tests" $ do
        specify "test 1" $ example $
          length test `shouldBe` 16
