module DisjointSet
 ( DisjointSet
 , empty
 , fromList
 , insert
 , inSameSet
 , union
 ) where

-- without rank maintenance for simplicity

import qualified Data.Map.Strict as M
import Data.Foldable

{-
  TODO:

  - test cases on DisjointSet
  - what if we want to change the base monad?

-}

type DisjointSet a = M.Map a a

empty :: DisjointSet a
empty = M.empty

insert :: Ord a => a -> DisjointSet a -> DisjointSet a
insert v = M.alter f v
  where
    f Nothing = Just v
    f m@(Just _) = m

fromList :: Ord a => [a] -> DisjointSet a
fromList = foldl' (flip insert) empty

root :: Ord a => a -> DisjointSet a -> (a, DisjointSet a)
root v ds = case M.lookup v ds of
    Nothing -> (v, M.insert v v ds)
    Just parent ->
      if parent == v
        then (v,ds)
        else
          let (r,ds1) = root parent ds
          in (r, M.insert v r ds1)

inSameSet :: Ord a => a -> a -> DisjointSet a -> (Bool, DisjointSet a)
inSameSet x y ds = (rx == ry, ds2)
  where
    (rx, ds1) = root x ds
    (ry, ds2) = root y ds1

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
