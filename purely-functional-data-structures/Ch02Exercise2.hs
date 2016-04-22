module Ch02Exercise2 where

{-

In book it says "in the worst case, `member` performs approximately 2d comparisons".

This is because in book, the implementation of `member` considers 3 branches:

if x < y then _branch1 else if x > y then _branch2 else _branch3

in the worst case we need to traverse to the deepest element of the tree,
by always taking _branch2, that results in 2d comparisons.

-}
