module DisjointSetState
  ( initM
  , includeElemM
  , inSameSetM
  , unionM
  ) where

import DisjointSet
import Control.Monad.State
-- import Control.Monad.Morph

{-
  NOTE:
  - if we want to change the base monad (Identity) to something else,
    for example Maybe, mmorph (https://hackage.haskell.org/package/mmorph)
    would be one that does the trick.
    the following code type checks:

> initM' :: Ord a => [a] -> StateT (DisjointSet a) Maybe ()
> initM' = hoist generalize . initM

-}

initM :: Ord a => [a] -> State (DisjointSet a) ()
initM = put . includeElems empty

inSameSetM :: Ord a => a -> a -> State (DisjointSet a) Bool
inSameSetM x y = state (inSameSet x y)

{-# ANN unionM "HLint: ignore Use infix" #-}
unionM :: Ord a => a -> a -> State (DisjointSet a) ()
unionM x y = modify (union x y)

includeElemM :: Ord a => a -> State (DisjointSet a) ()
includeElemM v = modify (includeElem v)
