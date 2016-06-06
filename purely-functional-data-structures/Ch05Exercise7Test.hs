module Ch05Exercise7Test where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Foldable

import Ch05Splay
import Ch05Exercise7

{-# ANN module "HLint: ignore Redundant do" #-}

main :: IO ()
main = hspec $ do
    describe "Splay.toInorderList" $ do
      it "produces the same list as toAscList does" $
        property $ \xs -> let t = fromList (xs :: [Int])
                          in toAscList t == toInorderList t
