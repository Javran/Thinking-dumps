module Problem83Test where


import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import Graph
import Problem83

{-# ANN module "HLint: ignore Redundant do" #-}

fromRaw :: String -> GraphForm Char (Edge Char)
fromRaw raw = GraphForm vertices (S.fromList edges)
  where
    edges = map parseEdge . words $ raw
    parseEdge (a:b:_) = Edge a b
    parseEdge _ = error "bad list"
    vertices = S.fromList (concatMap (\(Edge a b) -> [a,b]) edges)

-- "g1" stands for the example graph referred to as "k4" in:
-- https://wiki.haskell.org/99_questions/Solutions/83
g1 :: GraphForm Char (Edge Char)
g1 = fromRaw "ab bc cd da ac bd"

-- see: http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
--- http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p83.gif
g2 :: GraphForm Char (Edge Char)
g2 = fromRaw "ab bc ce eh hg gf fd da de be dg"

main = hspec $ do
    describe "random tests" $ do
        specify "test 1" $ example $
          length (spantree g1) `shouldBe` 16
