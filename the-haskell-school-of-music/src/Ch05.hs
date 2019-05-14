{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch05 where

import Data.Function
import Data.Semigroup
import Euterpea

import qualified Ch01

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

{- Ex 5.4 -}
apPairs :: [AbsPitch] -> [AbsPitch] -> [(AbsPitch, AbsPitch)]
apPairs xs ys = [(x,y) | x <- xs, y <- ys, let diff = x - y, diff > 2, diff < 8]

{-
  Ex 5.5

  just point-free refactor this. I wouldn't recommend spending time
  understandng this point-less form.

 -}
hList' :: Dur -> [Pitch] -> Music Pitch
hList' = (line .) . map . Ch01.hNote (-3)

{- Ex 5.6 is already done in Ch04 -}

{- Ex 5.7 -}
ex5_7 :: [Double] -> [Double]
ex5_7 = (((/2) . (+1)) <$>)


{-
  Ex 5.8

  map f (map g xs) === map (f . g) xs [map fusion]

  and:

  map (\x -> (x+1)/2) xs
  => map (/2) (map (+1) xs)

 -}

{- Ex 5.9 skipped as I've been using the standard practice -}

{-
  Ex 5.10

  [5,10,15,20]
  => [(*) 1 5, (*) 2 5, (*) 3 5, (*) 4 5]
  => f1 [(*) 1, (*) 2, (*) 3, (*) 4] 5 = [(*) 1 5, (*) 2 5, (*) 3 5, (*) 4 5]
  => f1 = flip (map . flip id)
  => f2 (*) [1,2,3,4] = [(*) 1, (*) 2, (*) 3, (*) 4]
  => f2 = map

 -}
ex5_10_Check :: Bool
ex5_10_Check = f1 (f2 (*) [1,2,3,4]) 5 == [5,10,15,20 ::Int]
  where
    -- note that with same type signature, `id` and `($)` are the same thing.
    f1 = flip (map . flip id)
    f2 = map
