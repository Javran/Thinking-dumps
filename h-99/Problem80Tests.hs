{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Control.Applicative
import Data.List
import Control.Monad
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Problem80 hiding (main)

newtype Vertex = Vertex Char deriving (Eq,Show,Ord)

genVertex :: Gen Vertex
genVertex = Vertex <$> elements ( ['a'..'z']
                               ++ ['A'..'Z']
                               ++ ['0'..'9']
                                )

subsetOf :: [a] -> Gen [a]
subsetOf = foldM go [] . reverse
    where
      go acc i = do
          b <- arbitrary
          return (if b then i:acc else acc)

takeOne :: [a] -> [(a,[a])]
takeOne [] = []
takeOne (x:xs) = (x,xs) : map (second (x:)) (takeOne xs)

takeOneM :: [a] -> Gen (a,[a])
takeOneM = elements . takeOne

shuffled :: [a] -> Gen [a]
shuffled [] = return []
shuffled xs = do
    (y,ys) <- takeOneM xs
    (y:) <$> shuffled ys

instance Arbitrary (GraphForm Vertex (Edge Vertex)) where
    arbitrary = do
        vs <- nub <$> listOf1 genVertex
        let n = length vs
        edgeSize <- choose (0,n*n)
        es <- resize edgeSize $ listOf ( Edge <$> elements vs <*> elements vs)
        return (GraphForm (S.fromList vs) (S.fromList es))

instance Arbitrary (AdjForm Vertex (Edge Vertex)) where
    arbitrary = do
        vs <- nub <$> listOf1 genVertex
        let n = length vs
        edgeSize <- choose (0,n*n)
        let adjMaps :: M.Map Vertex (S.Set (Edge Vertex))
            adjMaps = undefined
        return . AdjForm $ adjMaps

instance Arbitrary (FndForm Vertex (Edge Vertex)) where
    arbitrary = do
        vs <- listOf1 genVertex
        let n = length vs
        edgeSize <- choose (0,n*n)
        es <- resize edgeSize $ listOf ( Edge <$> elements vs <*> elements vs)
        return $ FndForm (map Left vs ++ map Right es)

prop_GraphFormToAdjForm :: GraphForm Vertex (Edge Vertex) -> Property
prop_GraphFormToAdjForm g = g === (adjFormToGraphForm . graphFormToAdjForm) g

prop_AdjFormToGraphForm :: AdjForm Vertex (Edge Vertex) -> Property
prop_AdjFormToGraphForm g = g === (graphFormToAdjForm . adjFormToGraphForm) g

main :: IO ()
main = sample $ shuffled ([]  :: [Int])

