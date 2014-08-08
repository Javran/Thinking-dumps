{-# LANGUAGE DeriveFoldable #-}
module LinkedList
where

import Prelude hiding (foldl,foldr)
import Data.Foldable

data LinkedList a
    = Nil
    | Cons a (LinkedList a)
      deriving (Foldable)

nil :: LinkedList a
nil = Nil

new :: a -> LinkedList a -> LinkedList a
new = Cons

isNil :: LinkedList a -> Bool
isNil Nil = True
isNil _ = False

datum :: LinkedList a -> a
datum (Cons v _) = v
datum _ = error "datum: empty LinkedList"

next :: LinkedList a -> LinkedList a
next (Cons _ n) = n
next _ = error "next: empty LinkedList"

fromList :: [a] -> LinkedList a
fromList = foldr Cons Nil

toList :: LinkedList a -> [a]
toList = foldr (:) []

reverseLinkedList :: LinkedList a -> LinkedList a
reverseLinkedList = foldl (flip Cons) Nil
