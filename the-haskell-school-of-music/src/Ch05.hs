{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch05 where

import Data.Function
import Data.Semigroup

{- Ex 5.1 -}
twice :: (a -> a) -> (a -> a)
twice f = f . f

{-
  Ex 5.2

  TODO: define something useful by power.

 -}
power :: (a -> a) -> Int -> (a -> a)
power f n = appEndo $ stimesMonoid n (Endo f)

{-
  Ex 5.3

  let's say:

  - f :: forall a b. a -> b
  - fix :: forall u v. u -> v

  let's pretend a,b,u,v are concrete types:

  - fix :: u -> v, f :: a -> b, u ~ (a -> b)
  => fix f :: v
  - note that fix f :: a
  => a ~ v
  - f (fix f :: a) :: v
  - since f :: a -> b, v ~ b ~ a

  therefore we've arrived at: fix :: (a -> a) -> a, turns out
  putting forall back is all we need:

  fix :: forall a. (a -> a) -> a

 -}
remainder, remainder' :: Integer -> Integer -> Integer

remainder a b = if a < b then a else remainder (a-b) b
remainder' = fix $ \f a b -> if a < b then a else f (a-b) b

remaindersAgree :: Bool
remaindersAgree = and
  [ remainder a b == remainder' a b | a <- [1..10], b <- [1..10] ]
