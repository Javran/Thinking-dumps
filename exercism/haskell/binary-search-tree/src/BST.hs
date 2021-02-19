{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}

module BST
  ( BST
  , bstLeft
  , bstRight
  , bstValue
  , empty
  , fromList
  , insert
  , singleton
  , toList -- exporting from Foldable.
  )
where

import Data.Foldable

data BST a
  = Node (BST a) a (BST a)
  | Nil
  deriving (Eq, Show, Foldable)

bstAlg :: (BST a -> a -> BST a -> r) -> r -> BST a -> r
bstAlg withNode withNil = \case
  Nil -> withNil
  Node l v r -> withNode l v r

bstLeft :: BST a -> Maybe (BST a)
bstValue :: BST a -> Maybe a
bstRight :: BST a -> Maybe (BST a)
(bstLeft, bstValue, bstRight) =
  ( bstAlg (\l _ _ -> Just l) Nothing
  , bstAlg (\_ v _ -> Just v) Nothing
  , bstAlg (\_ _ r -> Just r) Nothing
  )

empty :: BST a
empty = Nil

insert :: Ord a => a -> BST a -> BST a
insert x =
  bstAlg
    (\l v r ->
       if x <= v
         then Node (insert x l) v r
         else Node l v (insert x r))
    (singleton x)

singleton :: a -> BST a
singleton x = Node Nil x Nil

fromList :: Ord a => [a] -> BST a
fromList = foldr insert empty . reverse
