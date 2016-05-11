module Ch03Exercise7 where

{-

the exercise asks us to achieve O(1) time findMin by
keeping a copy of minimum elements outside of the heap.

by doing so we are actually keeping two heaps: one is the regular one,
and the other is a heap that contains at most one element.

about implementations:

* insert: updates the minimum if possible
* merge: take the minimum of two
* deleteMin: perform action on original heap first, then
  store current minimum value

TODO: lack interest on this, will complete this later

-}
