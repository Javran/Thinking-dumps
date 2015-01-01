{-# LANGUAGE FlexibleInstances, TupleSections, TemplateHaskell #-}

import Test.QuickCheck
import Control.Applicative
import Data.List
import Control.Monad
import Control.Arrow
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Problem80 hiding (main)

-- | vertex are just chars
newtype Vertex = Vertex Char deriving (Eq,Show,Ord)

-- | generates a random vertex
genVertex :: Gen Vertex
genVertex = Vertex <$> elements ( ['a'..'z']
                               ++ ['A'..'Z']
                               ++ ['0'..'9']
                                )

-- | generates a subset of the given list
subsetOf :: [a] -> Gen [a]
subsetOf = foldM go [] . reverse
    where
      go acc i = do
          b <- arbitrary
          return (if b then i:acc else acc)

-- | generates all information requried for a graph
--   including a list of unique vertices and a list all the edges
genRawGraph :: Gen ([Vertex], [Edge Vertex])
genRawGraph = do
    vs <- nub <$> listOf genVertex
    es <- subsetOf [Edge v1 v2 | v1 <- vs, v2 <- vs]
    return (vs,es)

-- | duplicates a value one to n times
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
prop_GraphFormToAdjForm = (===) <$> id
                                <*> adjFormToGraphForm
                                  . graphFormToAdjForm


prop_AdjFormToGraphForm :: AdjForm Vertex (Edge Vertex) -> Property
prop_AdjFormToGraphForm = (===) <$> id
                                <*> graphFormToAdjForm
                                  . adjFormToGraphForm

prop_GraphFormToFndForm :: GraphForm Vertex (Edge Vertex) -> Property
prop_GraphFormToFndForm = (===) <$> id
                                <*> fndFormToGraphForm
                                  . graphFormToFndForm

prop_FndFormToGraphForm :: FndForm Vertex (Edge Vertex) -> Property
prop_FndFormToGraphForm = (===) <$> fndFormToGraphForm
                                <*> fndFormToGraphForm
                                  . graphFormToFndForm
                                  . fndFormToGraphForm

return []
main :: IO Bool
main = $quickCheckAll
