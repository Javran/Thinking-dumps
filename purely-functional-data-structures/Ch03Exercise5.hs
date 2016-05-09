module Ch03Exercise5 where

import Test.Hspec
import Test.QuickCheck

import Ch03Binomial hiding (findMin)

{-# ANN module "HLint: ignore Redundant do" #-}
-- exercise: define "findMin" directly rather than via a call to "viewMinTree"

-- again I'm not sure what's the point of this exercise,
-- as it's just a process of replacing terms with their definitions.
-- but let's do this anyway.
findMin :: Ord a => Heap a -> Maybe a
findMin [Node _ x _] = Just x
findMin (t:ts) = do
    v2 <- findMin ts
    Just $ if root t <= v2 then root t else v2
findMin [] = Nothing

main :: IO ()
main = hspec $ do
    describe "Binomial" $ do
      it "find the minimal element correctly" $ do
        let getMin :: Ord a => [a] -> Maybe a
            getMin [] = Nothing
            getMin xs = Just $ minimum xs
        property $ \xs -> (findMin . fromList) (xs :: [Int]) == getMin xs
