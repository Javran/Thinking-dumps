module DisjointSet where

import qualified Data.Map.Strict as M
import Control.Monad.State

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

initM :: Ord a => [a] -> State (DisjointSet a) ()
initM = put . mkSet

getRootM :: Ord a => a -> State (DisjointSet a) a
getRootM v = do
    result <- gets (getRoot v)
    case result of
      Just (r,ds) -> put ds >> pure r
      Nothing -> fail "getRootM: element not found"

{-# ANN unionM "HLint: ignore Use infix" #-}
unionM :: Ord a => a -> a -> State (DisjointSet a) ()
unionM x y = modify (union x y)
