{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TupleSections, ConstraintKinds #-}
module Graph where

import Test.QuickCheck
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Function
import Data.List

import qualified Data.Map.Strict as M
import qualified Data.Set as S

{-
    for a graph we can safely assume that every edge is unique,
    and that every vertex is uniquely distinguished by its name.
    note that these assumption do allow multiple edges between two vertices:
    edges can have extra data to make them different despite that whose
    terminals might be exactly the same.
-}

{-
    graph-term form

    - all vertices must be present
    - all edges must be present
-}
data GraphForm v e = GraphForm
    { gfVertices :: S.Set v
    , gfEdges    :: S.Set e
    } deriving (Show,Eq)

{-
    adjacency-list form

    - a map from vertices to its neighboring vertices
    - here we modify the presentation a little bit:
      instead of storing neighboring vertices, we store
      neighboring edges.
    - the original adjacency-list form can be recovered easily
    - edges can store more information than vertices
      and actually the origin adjacency-list form fails to
      keep enough information to recover the edge if the edge has
      extra data.
-}
data AdjForm v e = AdjForm (M.Map v (S.Set e)) deriving (Show,Eq)

{-
    human-friendly form

    - a list of either vertices (Left) or edges (Right)
    - duplicate elements are allowed (but will be compared and eliminated
      when converting
-}
data FndForm v e = FndForm [Either v e] deriving (Show,Eq)

-- | "OrdVE v e" defines the relation between vertex type "v"
-- and edge type "e". and they are both required to be a instance of Ord
type OrdVE v e = (Ord v, Ord e, VertexEdge v e)

-- | "Edge" does not allow more than one edge between any pair of vertices
--   also it's undirected
data Edge v = Edge v v deriving Show

instance Eq v => Eq (Edge v) where
    (Edge a b) == (Edge c d) = (a,b) == (c,d) || (a,b) == (d,c)

instance Ord v => Ord (Edge v) where
    compare = compare `on` normalize
        where
          normalize (Edge a b)
              | a <= b = (a,b)
              | otherwise = (b,a)

-- | the relation between vertex and edge
--   minimal implementation: terminals
class Eq v => VertexEdge v e where
    -- edges have two terminals
    terminals :: e -> (v,v)
    -- can test if a vertex is one of its terminal
    terminalOf :: v -> e -> Bool

    v `terminalOf` e = let (a,b) = terminals e
                       in v == a || v == b

instance Eq v => VertexEdge v (Edge v) where
    terminals (Edge a b) = (a,b)

-- | generates a random vertex
genVertex :: Gen Char
genVertex = elements ( ['a'..'z']
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
genRawGraph :: Gen ([] Char, [Edge Char])
-- I'm using this kind-of-weird type signature to emphasize this is
-- a list of chars rather than a string.
genRawGraph = do
    vs <- nub <$> listOf genVertex
    es <- subsetOf [Edge v1 v2 | v1 <- vs, v2 <- vs]
    return (vs,es)

-- | duplicates a value one to n times
randomDuplicates :: Int -> a -> Gen [a]
randomDuplicates n x = flip replicate x <$> choose (1,n)

instance Arbitrary (GraphForm Char (Edge Char)) where
    arbitrary = uncurry GraphForm
              . (S.fromList *** S.fromList)
             <$> genRawGraph

instance Arbitrary (AdjForm Char (Edge Char)) where
    arbitrary = do
        (_,es) <- genRawGraph
        let splitEdge e@(Edge v1 v2) = [(v1,e),(v2,e)]
            pairs = map (second S.singleton)
                  . concatMap splitEdge $ es
        return . AdjForm . M.fromListWith S.union $ pairs

instance Arbitrary (FndForm Char (Edge Char)) where
    arbitrary = do
        (vs,es) <- genRawGraph
        let ves = map Left vs ++ map Right es
        vesDup <- concat <$> mapM (randomDuplicates 5) ves
        return $ FndForm vesDup
