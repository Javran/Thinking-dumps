module Ch05Exercise4Test where

import Ch05Splay
import Ch05Exercise4

-- putting things together
insert :: Ord a => a -> Tree a -> Tree a
insert x t = T (smaller x t) x (bigger x t)

-- converting a splay tree into a list
toAscList :: Tree a -> [a]
toAscList t = case viewMin t of
    Nothing -> []
    Just (v,t') -> v : toAscList t'
