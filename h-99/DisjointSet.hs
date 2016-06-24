module DisjointSet where

import qualified Data.Map.Strict as M

type DisjointSet a = M.Map a a

mkSet :: Ord a => [a] -> DisjointSet a
mkSet = M.fromList . map (\x -> (x,x))

getRoot :: Ord a => a -> DisjointSet a -> Maybe (a, DisjointSet a)
getRoot v ds = do
    parent <- M.lookup v ds
    if parent == v
       then pure (v,ds)
       else do
         (r,ds1) <- getRoot parent ds
         pure (r, M.insert v r ds1)

{-# ANN union "HLint: ignore Eta reduce" #-}
union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y ds = M.insert x y ds
