module Ch03Exercise10 where

import Ch03RedBlack

lBalance :: Color -> Tree a -> a -> Tree a -> Tree a
lBalance Black (T Red (T Red a x b) y c) z d = T Red (T Black a x b) y (T Black c z d)
lBalance Black (T Red a x (T Red b y c)) z d = T Red (T Black a x b) y (T Black c z d)
lBalance c l v r = T c l v r
