module Ch05SplayTest where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)
import Data.Foldable
import Control.Monad

import Ch05Splay

{-# ANN module "HLint: ignore Redundant do" #-}

fromList :: Ord a => [a] -> Tree a
fromList = foldl' (flip insert) empty

checkTree :: Ord a => Tree a -> Maybe ()
checkTree E = pure ()
checkTree (T a x b) = do
    case a of
        E -> pure ()
        T _ ra _ -> guard $ ra <= x
    case b of
        E -> pure ()
        T _ rb _ -> guard $ rb > x
    checkTree a
    checkTree b
