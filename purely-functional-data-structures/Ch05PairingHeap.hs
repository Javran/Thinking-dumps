module Ch05PairingHeap where

data Heap a
  = E
  | T a [Heap a] -- INVARIANT: all elements in this list should be non-empty
