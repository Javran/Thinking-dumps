{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module DisjointSet
 ( DisjointSet
 , empty
 , includeElems
 , includeElem
 , inSameSet
 , union
 , toGroups
 ) where

-- without rank maintenance for simplicity (TODO in future?)

import qualified Data.Map.Strict as M
import Data.Foldable
import Control.Monad.State

{-
  - you can view DisjointSet as a container that might contain
    many small and disjoint regular sets (i.e. no two set can share the
    same element)

  - DisjointSet allows to union two of these small sets or
    query whether two elements belong to the same small set
    efficiently

  - "includeElem" "includeElems" simply registers the existence of a value
    in the disjoint set, and every newly added elements to DisjointSet
    is a singleton set in itself, if the same element haven't been added before.

  - the reason that we have "includeElem" and "includeElems" is to make it more
    convenient when dealing with graphs: it is possible for a group to have
    vertices that don't have any edge at all. in this case simply register "union"
    relation in the disjoint set won't cover these vertices.
-}

-- see also: https://en.wikipedia.org/wiki/Disjoint-set_data_structure
type DisjointSet a = M.Map a (a,Int)

-- | an empty DisjointSet
empty :: DisjointSet a
empty = M.empty

-- | register the value in this DisjointSet.
includeElem :: Ord a => a -> DisjointSet a -> DisjointSet a
includeElem v = M.alter f v
  where
    f Nothing = Just (v,0)
    f m@(Just _) = m

-- | create a DisjointSet with some elements registered
includeElems :: Ord a => DisjointSet a -> [a] -> DisjointSet a
includeElems = foldl' (flip includeElem)

-- | (INTERNAL ONLY) get the root of current value in set
root :: Ord a => a -> DisjointSet a -> ((a,Int), DisjointSet a)
root v ds = case M.lookup v ds of
    Nothing -> let item = (v,0) in (item, M.insert v item ds)
    Just item@(parent,_) ->
      if parent == v
        then (item,ds)
        else
          let (r,ds1) = root parent ds
          in (r, M.insert v r ds1)

-- | check whether two values belong to the same set / root.
inSameSet :: Ord a => a -> a -> DisjointSet a -> (Bool, DisjointSet a)
inSameSet x y ds = (rx == ry, ds2)
  where
    (rx, ds1) = root x ds
    (ry, ds2) = root y ds1

-- | union the 2 sets containing the 2 values
{-# ANN union "HLint: ignore Eta reduce" #-}
union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y ds =
    let (b, ds1) = inSameSet x y ds
    in if b
         then ds1
         else
           let ((rx,rnkX),ds2) = root x ds1
               ((ry,rnkY),ds3) = root y ds2
           in case rnkX `compare` rnkY of
                LT -> M.insert rx (ry,rnkX) ds3
                GT -> M.insert ry (rx,rnkY) ds3
                EQ -> M.insert ry (rx,rnkX+1) ds3

toGroups :: forall a. Ord a => DisjointSet a -> [ [a] ]
toGroups ds = M.elems (evalState (foldM updateM M.empty (M.keys ds :: [a])) ds)
  where
    updateM :: M.Map a [a] -> a -> State (DisjointSet a) (M.Map a [a])
    updateM rootMap k = do
        (r,_) <- state (root k)
        let alt Nothing = Just [k]
            alt (Just ks) = Just (k:ks)
        pure (M.alter alt r rootMap)
