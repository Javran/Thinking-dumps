module Leftist
    ( LTree
    , merge
    , empty
    , null
    , singleton
    , insert
    , deleteMin
    , toList
    , toAscList
    , fromList
    , sort
    , propertyHolds
    )
where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq, (|>), ViewL(..), viewl)
import Data.Function
import Prelude hiding (null)

data LTree a
    = Nil
    | Node !Int !a !(LTree a) !(LTree a)
      deriving (Show)

-- | node distance
dist :: LTree a -> Int
dist Nil = -1
dist (Node d _ _ _) = d

-- | node label
value :: LTree a -> a
value Nil = error "value on Nil"
value (Node _ v _ _) = v

-- | /O(1)/ the empty leftist tree
empty :: LTree a
empty = Nil

-- | /O(1)/ is the leftist tree empty?
null :: LTree a -> Bool
null Nil = True
null _ = False

-- | /O(1)/ a singleton leftist tree
singleton :: a -> LTree a
singleton x = Node 0 x Nil Nil

-- | /O(log(n))/ merge two leftist trees
merge :: Ord a => LTree a -> LTree a -> LTree a
merge Nil r = r
merge l Nil = l
merge ta tb = Node newDist v1 newL newR
    where
        -- property: ta and tb are both non-empty
        (treeL,treeR) = if value ta <= value tb then (ta,tb) else (tb,ta)
        -- property: value treeL <= value treeR
        (Node _ v1 l1 r1) = treeL
        r1' = merge r1 treeR
        (newL,newR) = if dist r1' >= dist l1 then (r1',l1) else (l1,r1')
        -- property: dist newL >= dist newR
        newDist = 1 + dist newR
        -- property: dist newNode = min{ dist newL, dist newR } + 1
        --                        = dist newR + 1

-- | /O(log(n)/ insert a node
insert :: Ord a => a -> LTree a -> LTree a
insert v = merge (singleton v)

-- | /O(log(n))/ delete the minimum
deleteMin :: Ord a => LTree a -> (a, LTree a)
deleteMin Nil = error "empty tree"
deleteMin (Node _ v l r) = (v, merge l r)

-- | /O(n log(n)/ convert to the leftist tree to an ascending list of elements
toAscList :: Ord a => LTree a -> [a]
toAscList Nil = []
toAscList t =
    let (v,t') = deleteMin t
    in v:toAscList t'

-- | convert it to a list
toList :: LTree a -> [a]
toList Nil = []
toList (Node _ v l r) = v : ((++) `on` toList) l r

-- | /O(n)/ convert from a list
fromList :: Ord a => [a] -> LTree a
fromList = fromSeq . fmap singleton . Seq.fromList

fromSeq :: Ord a => Seq (LTree a) -> LTree a
fromSeq s = case viewl s of
    EmptyL -> Nil
    v1 :< seq1 -> case viewl seq1 of
        EmptyL -> v1
        -- for seq with more than 2 elements,
        -- merge the first two elements and enqueue the resulting tree
        v2 :< seq2 -> fromSeq (seq2 |> merge v1 v2)

-- | /O(n log(n))/ sort a list using leftist tree
sort :: Ord a => [a] -> [a]
sort = toAscList . fromList

-- | check if certain property holds for leftist tree
propertyHolds :: Ord a => LTree a -> Bool
propertyHolds Nil = True
propertyHolds (Node d v l r) =
       ((&&) `on` propertyHolds) l r -- property holds recursively
    && ((>=) `on` dist) l r          -- dist l >= dist r
    && (d == 1 + dist r)             -- dist a = 1 + dist( leftChild(a) )
    && (null l || v <= value l)      -- leftist tree viewed as a min-heap
    && (null r || v <= value r)
