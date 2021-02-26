{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module Zipper
  ( BinTree (..)
  , Zipper
  , fromTree
  , toTree
  , value
  , left
  , right
  , up
  , setValue
  , setLeft
  , setRight
  )
where

import Data.List

-- | A binary tree.
data BinTree a = BT
  { -- | Value
    btValue :: a
  , -- | Left child
    btLeft :: Maybe (BinTree a)
  , -- | Right child
    btRight :: Maybe (BinTree a)
  }
  deriving (Eq, Show)

newtype Dir = Dir Bool deriving (Eq, Show) via Bool

pattern LBranch :: Dir
pattern LBranch = Dir False

pattern RBranch :: Dir
pattern RBranch = Dir True

{-# COMPLETE LBranch, RBranch #-}

-- (a, LBranch, tree) for context: (<focus>, btValue, tree)
-- (a, RBranch, tree) for context: (tree, btValue, <focus>)
type BinTreeContext a = (a, Dir, Maybe (BinTree a))

-- | A zipper for a binary tree.
data Zipper a = Zipper
  { -- | the binary tree under focus
    zFocus :: BinTree a
  , -- | a stack of contexts
    zContext :: [BinTreeContext a]
  }
  deriving (Eq, Show)

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

applyContext :: BinTreeContext a -> BinTree a -> BinTree a
applyContext (v, dir, t) tFocus = case dir of
  LBranch -> BT v (Just tFocus) t
  RBranch -> BT v t (Just tFocus)

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper tFocus ctxt) = foldl' (flip applyContext) tFocus ctxt

-- | Get the value of the focus node.
value :: Zipper a -> a
value = btValue . zFocus

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left Zipper {zFocus=BT v ml mr, zContext} = do
  l <- ml
  pure $ Zipper l ((v, LBranch, mr) : zContext)

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right Zipper {zFocus=BT v ml mr, zContext} = do
  r <- mr
  pure $ Zipper r ((v, RBranch, ml) : zContext)

-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up (Zipper t ctxt) = do
  (x : xs) <- pure ctxt
  pure $ Zipper (applyContext x t) xs

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue v (Zipper (BT _ l r) ctxt) = Zipper (BT v l r) ctxt

-- | Replace a left child tree.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft l (Zipper (BT v _ r) ctxt) = Zipper (BT v l r) ctxt

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight r (Zipper (BT v l _) ctxt) = Zipper (BT v l r) ctxt
