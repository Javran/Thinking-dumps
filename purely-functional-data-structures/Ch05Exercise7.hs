module Ch05Exercise7 where

import Ch05Splay

toInorderList :: Tree a -> [a]
toInorderList E = []
toInorderList (T a x b) = toInorderList a ++ x : toInorderList b
