{-# LANGUAGE TypeSynonymInstances #-}
module CustomSet
  ( CustomSet
  , empty
  , delete
  , difference
  , isDisjointFrom
  , null
  , intersection
  , member
  , insert
  , size
  , isSubsetOf
  , fromList
  , toList
  , union
  ) where

-- not having too much good idea, let's begin with a sorted list
type CustomSet a = [a]

-- TODO: a custom Show instance .. do we have to use newtype?

empty :: CustomSet a
empty = []

delete :: Ord a => a -> CustomSet a -> CustomSet a
delete _ [] = []
delete v ls@(x:xs) = case compare v x of
    LT -> ls
    EQ -> xs
    GT -> x : delete v xs

toList :: CustomSet a -> [a]
toList = id

fromList :: Ord a => [a] -> CustomSet a
fromList = foldr insert []

member :: Ord a => a -> CustomSet a -> Bool
member _ [] = False
member v (x:xs) = case compare v x of
    LT -> False
    EQ -> True
    GT -> member v xs

insert :: Ord a => a -> CustomSet a -> CustomSet a
insert v [] = [v]
insert v ls@(x:xs) = case compare v x of
    LT -> v:ls
    EQ -> ls
    GT -> x : insert v xs

size :: CustomSet a -> Int
size = length

union :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
union = foldr insert

-- TODO: there are more efficient impl
difference :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
difference = foldr delete

isDisjointFrom :: Ord a => CustomSet a -> CustomSet a -> Bool
isDisjointFrom xs ys = length xs + length ys == length (xs `union` ys)

intersection :: Ord a => CustomSet a -> CustomSet a -> CustomSet a
intersection [] _ = []
intersection _ [] = []
intersection as@(x:xs) bs@(y:ys) = case compare x y of
    LT -> intersection xs bs
    EQ -> x : intersection xs ys
    GT -> intersection as ys

isSubsetOf :: Ord a => CustomSet a -> CustomSet a -> Bool
isSubsetOf xs ys = null (xs `difference` ys)
