module BST
  ( bstLeft, bstRight, bstValue
  , singleton
  , insert
  , fromList
  , toList -- from Data.Foldable
  ) where

import Data.Foldable
import Data.Monoid
import Data.Maybe

-- from testcases, it seems this BST will always
-- contain at least one element,
-- so there is no "Nil" alternative constructor in this definition
data BinTree a = BinTree
  { btVal :: a
  , btLeft :: Maybe (BinTree a)
  , btRight :: Maybe (BinTree a)
  } deriving (Show)

bstLeft, bstRight :: BinTree a -> Maybe (BinTree a)
bstLeft = btLeft
bstRight = btRight

bstValue :: BinTree a -> a
bstValue = btVal

binTreeFoldMap :: Monoid m => (a -> m) -> BinTree a -> m
binTreeFoldMap f (BinTree v l r) =
       fromMaybe mempty (binTreeFoldMap f <$> l)
    <> f v
    <> fromMaybe mempty (binTreeFoldMap f <$> r)

instance Foldable BinTree where
    foldMap = binTreeFoldMap

singleton :: a -> BinTree a
singleton v = BinTree v Nothing Nothing

insert :: Ord a => a -> BinTree a -> BinTree a
insert newV bt@(BinTree v l r) =
    if newV <= v
      then
        -- update left branch
        bt { btLeft = Just $ maybe newLeaf (insert newV) l }
      else
        -- update right branch
        bt { btRight = Just $ maybe newLeaf (insert newV) r }
  where
    newLeaf = singleton newV

fromList :: Ord a => [a] -> BinTree a
fromList [] = error "source list cannot be empty"
fromList (x:xs) = foldl (flip insert) (singleton x) xs
