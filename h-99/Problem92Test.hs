module Problem92Test where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.Maybe
import Control.Monad
import qualified Data.List as L
import qualified Data.IntMap.Strict as IM
import Criterion.Main

import Problem92

{-# ANN module "HLint: ignore Redundant do" #-}

genTree :: Int -> Gen [Edge]
genTree n = genTree' 2
  where
    -- cur should start from 2
    genTree' cur
        | cur > n = pure []
        | otherwise = do
            prev <- choose (1,cur-1)
            es' <- genTree' (cur+1)
            pure $ (prev,cur) : es'

validate :: [Edge] -> [Int] -> Bool
validate es nodes =
       lengthCheck
    && isJust (do
         diffs <- mapM (uncurry getEdge) es
         guard $ L.sort diffs == [1..length es])
  where
    lengthCheck = length es == length nodes - 1
    assigns = IM.fromList (zip [1..] nodes)
    getEdge x y = do
        a <- IM.lookup x assigns
        b <- IM.lookup y assigns
        pure (abs (a-b))

mainTest :: IO ()
mainTest = hspec $ do
    describe "vonKoch" $ do
      specify "on given example" $ example $ do
          let es = [(1,6),(2,6),(3,6),(4,6),(5,6)
                   ,(5,7),(5,8),(8,9),(5,10),(10,11)
                   ,(11,12),(11,13),(13,14)]
              results = vonKoch es
          take 5 results `shouldSatisfy` all (validate es)
      specify "on random trees" $ do
          property $ do
              n <- choose (4,8)
              es <- genTree n
              let results = vonKoch es
              pure $ all (validate es) (take 2 results)

mainBenchmark :: IO ()
mainBenchmark = defaultMain
    [ env setupEnv (\ ~(g1,g2) ->
        bgroup "vonKoch"
          [ bench "small"  $ nf (map (take 1 . vonKoch)) g1
          , bench "large"  $ nf (map (take 1 . vonKoch)) g2
          ])
    ]
  where
    setupEnv = do
        let seed = 334308585
            genSmallTrees = replicateM 5 (genTree 10)
            genLargeTrees = replicateM 2 (genTree 12)
            (MkGen genAll) = (,) <$> genSmallTrees <*> genLargeTrees
            result = genAll (mkQCGen seed) 30
        pure result

-- stack exec -- ghc Problem92Test.hs -O2 -main-is Problem92Test && ./Problem92Test
main :: IO ()
main = do
    mainTest
    -- mainBenchmark
