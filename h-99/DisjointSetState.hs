module DisjointSetState
  ( initM
  , includeElemM
  , inSameSetM
  , unionM
  ) where

import DisjointSet
import Control.Monad.State

initM :: Ord a => [a] -> State (DisjointSet a) ()
initM = put . includeElems empty

inSameSetM :: Ord a => a -> a -> State (DisjointSet a) Bool
inSameSetM x y = state (inSameSet x y)

{-# ANN unionM "HLint: ignore Use infix" #-}
unionM :: Ord a => a -> a -> State (DisjointSet a) ()
unionM x y = modify (union x y)

includeElemM :: Ord a => a -> State (DisjointSet a) ()
includeElemM v = modify (includeElem v)
