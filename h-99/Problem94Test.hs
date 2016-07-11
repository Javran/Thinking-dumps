module Problem94Test where

import Test.Hspec

import Problem94

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "regular" $ do
        let testRegular n k expected =
                specify (unwords [ "regular", show n, show k
                                 , "==", show expected]) $ example $
                    length (regular n k) `shouldBe` expected
        testRegular 3 2 1

        testRegular 4 2 1
        testRegular 4 3 1

        testRegular 5 2 1
        testRegular 5 3 0
        testRegular 5 4 1

        testRegular 6 2 2
        testRegular 6 3 2
        testRegular 6 4 1
        testRegular 6 5 1

        testRegular 7 2 2
        testRegular 7 3 0
        testRegular 7 4 2
        testRegular 7 5 0
        testRegular 7 6 1

        -- testRegular 9 2 4
