module Problem91Test where


import Test.Hspec
import Test.QuickCheck
import qualified Data.Set as S
import Control.Monad
import Debug.Trace

import Problem91

{-# ANN module "HLint: ignore Redundant do" #-}

-- 2 coordinates are near if and only if
-- there is a direct knight move from one to the other.
near :: Coord -> Coord -> Bool
near (x1,y1) (x2,y2) = dist == 3 && dx /= 0 && dy /= 0
  where
    dx = abs (x1-x2)
    dy = abs (y1-y2)
    dist = dx + dy

validatePath :: [Coord] -> Bool
validatePath xs = and $ zipWith near xs (tail xs)

validateTour :: Int -> [Coord] -> Bool
validateTour n xs =
    -- order of conditions: we first confirm all cells are contained
    -- in the path, which also serves as a proof that the list is non-empty
    -- (when n > 0)
    S.fromList xs == allCells
 && validatePath xs
  where
    cs = [1..n]
    allCells = S.fromList [(x,y) | x <- cs, y <- cs]

main :: IO ()
main = hspec $ do
    describe "Problem91.knightsTo" $ do
      -- when we have n = 5 and some random target cells,
      -- the performance is not good.
      -- so we test by first fixing target cell to be (1,1)
      -- and then verify some fixed target cells on size 8x8
      let verifyOn n target = do
              let results = knightsTo n target
                  validate xs = validateTour n xs && last xs == target
              take 10 results `shouldSatisfy` all validate
      it "produces correct results on 5x5 size (up to first 10)" $
        verifyOn 5 (1,1)
      it "produces correct results on 8x8 size (up to first 10)" $
        verifyOn 8 (1,1)
      it "works on some random cells (for 8x8 size)" $ do
        verifyOn 8 (2,3)
        verifyOn 8 (4,6)
        verifyOn 8 (5,5)

    describe "Problem91.closedKnights" $ do
      it "produces closed tours" $ do
          let n = 8
              results = closedKnights n
              validate xs = validateTour n xs
                         && head xs `near` last xs
          take 10 results `shouldSatisfy` all validate
