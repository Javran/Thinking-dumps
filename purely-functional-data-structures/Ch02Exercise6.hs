module Ch02Exercise6 where

import Prelude hiding (lookup)
import Data.Maybe

-- the only thing we need is just the data type definition
-- actually there are not much thing we can reuse from UnbalancedSet (a.k.a Ch02BST):
-- it's hard to implement UnbalancedSet in terms of FiniteMap
-- but the other way around isn't too hard.
import Ch02BST (BST(..))

type FiniteMap k v = BST (k,v)

-- "member" can be implemented using "lookup" but not the
-- other way around.
lookup :: Ord a => a -> FiniteMap a b -> Maybe b
lookup _ E = Nothing
lookup x (T l (k,v) r)
    | x < k = lookup x l
    | x > k = lookup x r
    | x == k = Just v
    | otherwise = error "lookup: impossible"

member :: Ord a => a -> FiniteMap a b -> Bool
member x = isJust . lookup x

-- we cannot reuse "insert" from BST
-- simply because now every element is not just a key
-- but contains some extra data (value)
-- if we are inserting an existing key into this finite map
-- we will usually expect this new insertion to replace the old key-value pair
-- which does not happen in BST's implementation
insert :: Ord a => a -> b -> FiniteMap a b -> FiniteMap a b
insert k v E = T E (k,v) E
insert k v (T l e@(curK,_) r)
    | k < curK = T (insert k v l) e r
    | k > curK = T l e (insert k v r)
    | k == curK = T l (k,v) r
    | otherwise = error "insert: impossible"
