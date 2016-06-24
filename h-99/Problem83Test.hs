{-# LANGUAGE ScopedTypeVariables #-}
module Problem83Test where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import Data.Maybe

import Graph
import DisjointSet
import Problem83

{-# ANN module "HLint: ignore Redundant do" #-}

fromRaw :: String -> GraphForm Char (Edge Char)
fromRaw raw = GraphForm vertices (S.fromList edges)
  where
    edges = map parseEdge . words $ raw
    parseEdge (a:b:_) = Edge a b
    parseEdge _ = error "bad list"
    vertices = S.fromList (concatMap (\(Edge a b) -> [a,b]) edges)

isUniqueOrds :: Ord a => [a] -> Bool
isUniqueOrds xs = length xs == S.size (S.fromList xs)

-- "g1" stands for the example graph referred to as "k4" in:
-- https://wiki.haskell.org/99_questions/Solutions/83
g1 :: GraphForm Char (Edge Char)
g1 = fromRaw "ab bc cd da ac bd"

-- see: http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
--- http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p83.gif
g2 :: GraphForm Char (Edge Char)
g2 = fromRaw "ab bc ce eh hg gf fd da de be dg"

-- check whether a list of edges forms a tree
isATree :: forall a. Ord a => [Edge a] -> Bool
isATree es = evalState (foldM check True es) (mkSet (S.toList vertices))
  where
    vertices = S.fromList (concatMap (\(Edge a b) -> [a,b]) es)
    check False _ = pure False
    check _ (Edge a b) = do
        r <- inSameSetM a b
        if r
          then pure False
          else unionM a b >> pure True

main :: IO ()
main = hspec $ do
    describe "spantree" $ do
        let r1 = spantree g1
            r2 = spantree g2
        specify "g1 result length" $ example $
          length r1 `shouldBe` 16
        specify "unique results" $ example $ do
          r1 `shouldSatisfy` isUniqueOrds
          r2 `shouldSatisfy` isUniqueOrds
        -- TODO: verify the results are trees
