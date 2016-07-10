module Problem94 where

import Data.List
import Control.Monad
import Graph
import qualified Data.Set as S
import Data.Maybe
import qualified Data.Map.Strict as M

{-
  TODO:
  - generate k-regular graph that has n nodes
  - find ways to eliminate isomorphic cases (hope the existing answer to P85 works)
-}

-- pick one element from the list with context (remained elements).
-- unlike "Utils.pick", "pickOne" will cut all elements
-- in front of the chosen element.
pickOne :: [a] -> [(a,[a])]
pickOne = map (\(x:xs) -> (x,xs)) . init . tails

-- pick n elements from a list with context (remained elements).
-- the resulting sequence of the chosen elements is always a subsequence
-- of the original input.
-- e.g. it's possible for "pickN _ [1..10]" to return "[1,4,5,6..]"
-- as one of its result, but returning "[1,4,6,5]" is not possible
-- because there's no way for "6,5" to be a proper subsequence of "[1..10]"
pickN :: Int -> [a] -> [([a],[a])]
pickN 0 xs = [([],xs)]
pickN n xs = do
    (y,ys) <- pickOne xs
    (z,zs) <- pickN (n-1) ys
    pure (y:z,zs)

-- TODO: we are assuming the graph is properly initialized
-- so that every node has an entity (might point to an empty set)
genGraph :: Ord a => Int -> [a] -> AdjForm a (Edge a) -> [AdjForm a (Edge a)]
genGraph _ [] graph = pure graph
genGraph k (v:vs) (AdjForm g) = do
    -- INVARIANT "(v:vs)" should only include those that still don't have sufficient
    --    adjacent nodes.
    let curDeg = S.size $ fromJust $ M.lookup v g
    -- 2. choose e-k edges and connect them. where "e" is the number of
    --    existing adjacent nodes. (TODO update comment)
    (newAdjs,_) <- pickN (k-curDeg) vs
    let g1 = foldl' (flip (addEdge v)) g newAdjs
    -- TODO:
    -- 3. filter "vs" to meet the invariant (this time we only need to
    --    investigate newly update nodes)
    _
  where
    addEdge a b = M.adjust (S.insert e) a . M.adjust (S.insert e) b
      where
        e = Edge a b
