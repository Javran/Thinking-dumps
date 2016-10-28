module Raz where

type Level = Int
data Dir = L | R

data Tree a
  = Nil
  | Leaf a
  | Bin Level Int (Tree a) (Tree a)

data List a
  = LNil
  | LCons a (List a)
  | LLvl Level (List a)
  | LTr (Tree a) (List a)

data Zip a = Zip (List a) a (List a)

singleton :: a -> Zip a
singleton e = Zip LNil e LNil
