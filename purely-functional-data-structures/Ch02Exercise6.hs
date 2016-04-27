module Ch02Exercise6 where

import Prelude hiding (lookup)
import Data.Maybe
import Ch02BST (BST(..))

type FiniteMap k v = BST (k,v)

lookup :: Ord a => a -> FiniteMap a b -> Maybe b
lookup _ E = Nothing
lookup x (T l (k,v) r)
    | x < k = lookup x l
    | x > k = lookup x r
    | x == k = Just v
    | otherwise = error "lookup: impossible"

member :: Ord a => a -> FiniteMap a b -> Bool
member x = isJust . lookup x

insert :: Ord a => a -> b -> FiniteMap a b -> FiniteMap a b
insert k v E = T E (k,v) E
insert k v (T l e@(curK,_) r)
    | k < curK = T (insert k v l) e r
    | k > curK = T l e (insert k v r)
    | k == curK = T l (k,v) r
    | otherwise = error "insert: impossible"
