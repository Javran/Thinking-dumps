{-# LANGUAGE FlexibleInstances, TupleSections, TemplateHaskell #-}

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

genRawGraph :: Gen ([Vertex], [Edge Vertex])
genRawGraph = do
    vs <- nub <$> listOf genVertex
    es <- subsetOf [Edge v1 v2 | v1 <- vs, v2 <- vs]
    return (vs,es)

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

randomDuplicates :: Int -> a -> Gen [a]
randomDuplicates n x = flip replicate x <$> choose (1,n)

instance Arbitrary (GraphForm Vertex (Edge Vertex)) where
    arbitrary = uncurry GraphForm
              . (S.fromList *** S.fromList)
             <$> genRawGraph

instance Arbitrary (AdjForm Vertex (Edge Vertex)) where
    arbitrary = do
        (_,es) <- genRawGraph
        let splitEdge e@(Edge v1 v2) = [(v1,e),(v2,e)]
            pairs = map (second S.singleton)
                  . concatMap splitEdge $ es
        return . AdjForm . M.fromListWith S.union $ pairs

instance Arbitrary (FndForm Vertex (Edge Vertex)) where
    arbitrary = do
        (vs,es) <- genRawGraph
        let ves = map Left vs ++ map Right es
        vesDup <- concat <$> mapM (randomDuplicates 5) ves
        return $ FndForm vesDup

prop_GraphFormToAdjForm :: GraphForm Vertex (Edge Vertex) -> Property
prop_GraphFormToAdjForm g = g === (adjFormToGraphForm . graphFormToAdjForm) g

prop_AdjFormToGraphForm :: AdjForm Vertex (Edge Vertex) -> Property
prop_AdjFormToGraphForm g = g === (graphFormToAdjForm . adjFormToGraphForm) g

prop_GraphFormToFndForm :: GraphForm Vertex (Edge Vertex) -> Property
prop_GraphFormToFndForm g = g === (fndFormToGraphForm . graphFormToFndForm) g

prop_FndFormToGraphForm :: FndForm Vertex (Edge Vertex) -> Property
prop_FndFormToGraphForm g = fndFormToGraphForm g === ( fndFormToGraphForm
                                                     . graphFormToFndForm
                                                     . fndFormToGraphForm) g

return []
main :: IO Bool
main = $quickCheckAll
