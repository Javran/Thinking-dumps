module Ch02Exercise6 where

import Prelude hiding (lookup)
import Ch02BST (BST(..))

type FiniteMap k v = BST (k,v)

lookup :: Ord a => a -> FiniteMap a b -> Maybe b
lookup _ E = Nothing
lookup x (T l (k,v) r)
    | x < k = lookup x l
    | x > k = lookup x r
    | x == k = Just v
    | otherwise = error "lookup: impossible"
