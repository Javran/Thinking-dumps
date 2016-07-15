{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module DisjointSet
 ( DisjointSet
 , empty
 , fromList
 , insert
 , inSameSet
 , union
 , toGroups
 ) where

-- without rank maintenance for simplicity (TODO in future?)

import qualified Data.Map.Strict as M
import Data.Foldable
import Control.Monad.State

{-
  TODO:
  - what if we want to change the base monad?
  - hide "insert" and "fromList", it seems we don't
    really need them
-}

{-
  - you can view DisjointSet as a container that might contain
    many small and disjoint regular sets (i.e. no two set can share the
    same element)

  - DisjointSet allows to union two of these small sets or
    query whether two elements belong to the same small set
    efficiently

  - some function names in this module are a bit different than
    what they usually mean:

    - "insert" "fromList" simply registers the existence of a value
      in the disjoint set, and every newly added elements to DisjointSet
      is a singleton set in itself, if the same element haven't been added before.

    - these 2 functions above are really optional, as trying to union
      non-existing elements has the effect of inserting them.

-}

type DisjointSet a = M.Map a a

-- | an empty DisjointSet
empty :: DisjointSet a
empty = M.empty

-- | register the value in this DisjointSet.
insert :: Ord a => a -> DisjointSet a -> DisjointSet a
insert v = M.alter f v
  where
    f Nothing = Just v
    f m@(Just _) = m

-- | create a DisjointSet with some elements registered
fromList :: Ord a => [a] -> DisjointSet a
fromList = foldl' (flip insert) empty

-- | (INTERNAL ONLY) get the root of current value in set
root :: Ord a => a -> DisjointSet a -> (a, DisjointSet a)
root v ds = case M.lookup v ds of
    Nothing -> (v, M.insert v v ds)
    Just parent ->
      if parent == v
        then (v,ds)
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
           let (rx,ds2) = root x ds1
               (ry,ds3) = root y ds2
           in M.insert rx ry ds3

toGroups :: forall a. Ord a => DisjointSet a -> [ [a] ]
toGroups ds = M.elems (evalState (foldM updateM M.empty (M.keys ds :: [a])) ds)
  where
    updateM :: M.Map a [a] -> a -> State (DisjointSet a) (M.Map a [a])
    updateM rootMap k = do
        r <- state (root k)
        let alt Nothing = Just [k]
            alt (Just ks) = Just (k:ks)
        pure (M.alter alt r rootMap)
