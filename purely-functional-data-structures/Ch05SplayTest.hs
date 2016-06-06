module Ch05SplayTest where

import Test.Hspec
import Test.QuickCheck
import Control.Monad
import Data.Maybe

import Ch05Splay

{-# ANN module "HLint: ignore Redundant do" #-}

checkTree :: Ord a => Tree a -> Maybe ()
checkTree E = pure ()
checkTree (T a x b) = do
    case a of
        E -> pure ()
        T _ ra _ -> guard $ ra <= x
    case b of
        E -> pure ()
        T _ rb _ -> guard $ rb >= x
    checkTree a
    checkTree b

main :: IO ()
main = hspec $ do
    describe "Splay.insert" $ do
      it "maintains Splay tree property" $
        property $ \xs -> isJust . checkTree . fromList $ (xs :: [Int])
