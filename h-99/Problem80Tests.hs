{-# LANGUAGE FlexibleInstances #-}
import Test.QuickCheck
import Control.Applicative
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

import Problem80 hiding (main)

newtype Vertex = Vertex Char deriving (Eq,Show,Ord)

genVertex :: Gen Vertex
genVertex = Vertex <$> elements ( ['a'..'z']
                               ++ ['A'..'Z']
                               ++ ['0'..'9']
                                )

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
        let genEdges :: Vertex -> Gen (Vertex, S.Set (Edge Vertex))
            genEdges v1 = do
                v2s <- listOf (elements vs)
                return (v1, S.fromList (map (Edge v1) v2s))
        pairs <- mapM genEdges vs
        return . AdjForm . M.fromList $ pairs

instance Arbitrary (FndForm Vertex (Edge Vertex)) where
    arbitrary = do
        vs <- listOf1 genVertex
        let n = length vs
        edgeSize <- choose (0,n*n)
        es <- resize edgeSize $ listOf ( Edge <$> elements vs <*> elements vs)
        return $ FndForm (map Left vs ++ map Right es)

prop_Convert :: GraphForm Vertex (Edge Vertex) -> Bool
prop_Convert g = g == (adjFormToGraphForm . graphFormToAdjForm) g

main :: IO ()
main = quickCheck prop_Convert
