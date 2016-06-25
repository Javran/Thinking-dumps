module DisjointSet where

import qualified Data.Map.Strict as M
import Control.Monad.State
import Data.Maybe

{-
  TODO:

  - test cases on DisjointSet
  - allowing adding new element instead of having a full set
  - what if we want to change the base monad?

-}

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

inSameSet :: Ord a => a -> a -> DisjointSet a -> Maybe (Bool, DisjointSet a)
inSameSet x y ds = do
    (rx, ds1) <- getRoot x ds
    (ry, ds2) <- getRoot y ds1
    pure (rx == ry, ds2)

{-# ANN union "HLint: ignore Eta reduce" #-}
union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y ds =
    let Just (b, ds1) = inSameSet x y ds
    in if b
         then ds1
         else
           let Just (rx,ds2) = getRoot x ds1
               Just (ry,ds3) = getRoot y ds2
           in M.insert rx ry ds3

initM :: Ord a => [a] -> State (DisjointSet a) ()
initM = put . mkSet

getRootM :: Ord a => a -> State (DisjointSet a) a
getRootM v = state (fromJust . getRoot v)

inSameSetM :: Ord a => a -> a -> State (DisjointSet a) Bool
inSameSetM x y = state (fromJust . inSameSet x y)

{-# ANN unionM "HLint: ignore Use infix" #-}
unionM :: Ord a => a -> a -> State (DisjointSet a) ()
unionM x y = modify (union x y)

-- TODO: tests
