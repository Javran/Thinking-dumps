module Problem92Test where

import Test.Hspec
import Test.QuickCheck
import Data.Maybe
import Control.Monad
import qualified Data.List as L
import qualified Data.IntSet as IS
import qualified Data.IntMap.Strict as IM

import Problem92

{-# ANN module "HLint: ignore Redundant do" #-}

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

main :: IO ()
main = hspec $ do
    describe "vonKoch" $ do
      specify "on given example" $ example $ do
          let es = [(1,6),(2,6),(3,6),(4,6),(5,6)
                   ,(5,7),(5,8),(8,9),(5,10),(10,11)
                   ,(11,12),(11,13),(13,14)]
              results = vonKoch es
          take 10 results `shouldSatisfy` all (validate es)
