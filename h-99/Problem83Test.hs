module Problem83Test where


import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S

import Graph
import Problem83

{-# ANN module "HLint: ignore Redundant do" #-}

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

test = search S.empty k4Edges k4Vertices

main = hspec $ do
    describe "random tests" $ do
        specify "test 1" $ example $
          length test `shouldBe` 16
