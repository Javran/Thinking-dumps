module Zipper
  ( BinTree(..)
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
  ) where

import Data.List

-- | A binary tree.
data BinTree a = BT
  { btValue :: a -- ^ Value
  , btLeft  :: Maybe (BinTree a) -- ^ Left child
  , btRight :: Maybe (BinTree a) -- ^ Right child
  } deriving (Eq, Show)

type BinTreeContext a = (a, Bool, Maybe (BinTree a))
    -- (a, False, tree) for context: (<focus>, btValue, tree)
    -- (a, True, tree) for context: (tree, btValue, <focus>)


-- | A zipper for a binary tree.
data Zipper a = Zipper
  { zFocus :: BinTree a -- ^ the binary tree under focus
  , zContext :: [BinTreeContext a] -- ^ a stack of contexts
  }

-- | Get a zipper focussed on the root node.
fromTree :: BinTree a -> Zipper a
fromTree t = Zipper t []

-- I'm using case-expression on purpose
-- because here Bool values are used as markers
-- rather than expression true or false
{-# ANN applyContext "HLint: ignore Use if" #-}
applyContext :: BinTreeContext a -> BinTree a -> BinTree a
applyContext (v, dir, t) tFocus = case dir of
    False -> BT v (Just tFocus) t
    True  -> BT v t (Just tFocus)

-- | Get the complete tree from a zipper.
toTree :: Zipper a -> BinTree a
toTree (Zipper tFocus ctxt) = foldl' (flip applyContext) tFocus ctxt

-- | Get the value of the focus node.
value :: Zipper a -> a
value = btValue . zFocus

-- | Get the left child of the focus node, if any.
left :: Zipper a -> Maybe (Zipper a)
left = undefined

-- | Get the right child of the focus node, if any.
right :: Zipper a -> Maybe (Zipper a)
right = undefined

-- | Get the parent of the focus node, if any.
up :: Zipper a -> Maybe (Zipper a)
up = undefined

-- | Set the value of the focus node.
setValue :: a -> Zipper a -> Zipper a
setValue = undefined

-- | Replace a left child tree.
setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft = undefined

-- | Replace a right child tree.
setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight = undefined
