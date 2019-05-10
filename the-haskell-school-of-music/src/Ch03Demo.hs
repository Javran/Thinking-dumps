{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch03Demo where

import Control.Applicative
import Data.Monoid
import Data.Coerce
import Euterpea

import qualified Ch02Demo

{- Ex 3.1 -}
f1 :: Int -> [Pitch] -> [Pitch]
f1 = Ch02Demo.transM

f2 :: [Dur] -> [Music a]
f2 = fmap rest

f3 :: [Music Pitch] -> [Music Pitch]
f3 = fmap tr
  where
    tr :: Music Pitch -> Music Pitch
    tr mp = case mp of
      Prim p -> case p of
                  Note d v -> Prim (Note (d/2) v) :+: Prim (Rest (d/2))
                  Rest _ -> mp
      a :+: b -> tr a :+: tr b
      a :=: b -> tr a :=: tr b
      Modify c a -> Modify c (tr a)

{-
  Ex 3.2:

  flip (flip f) [need to extend this by application (pick x and y)]
  => flip (flip f) x y
  => flip f y x
  => f x y

  Ex 3.3:

  let i = Integer

  xs :: [] i
  ys = (map :: (i -> j) -> [] i -> [] j) ((+) :: i -> (i -> i)) (xs :: [] i)
     => map (+) xs :: [] (i -> i)
  ys :: [] (i -> i)

 -}

{- Ex 3.4 -}
applyEach :: Functor f => f (a -> b) -> a -> f b
applyEach xs a = fmap ($ a) xs

{- Ex 3.5 -}
applyAll :: Foldable f => f (a -> a) -> a -> a
applyAll xs z = foldr ($) z xs

{-
  Ex 3.6

  for those two functions:

  appendr [xs,ys,zs]
  => foldr (flip (++)) [] [xs,ys,zs]
  => (([] ++ zs) ++ ys) ++ xs

  appendl [xs,ys,zs]
  => foldl (flip (++)) [] [xs,ys,zs]
  => zs ++ (ys ++ (xs ++ []))

  and we already know right-associativity is more efficient in such cases.
 -}

{- Ex 3.7 -}
myLength :: Foldable f => f a -> Int
myLength = getSum . foldMap (const 1)

{- Ex 3.8 -}
doubleEach :: (Functor f, Num n) => f n -> f n
doubleEach = fmap (*2)

pairAndOne :: (Functor f, Enum n) => f n -> f (n,n)
pairAndOne = fmap (\x -> (x, succ x))

addEachPair :: (Functor f, Num n) => f (n,n) -> f n
addEachPair = fmap (uncurry (+))

addPairsPointwise :: (Foldable f, Num m, Num n) => f (m,n) -> (m,n)
addPairsPointwise = (getSum *** getSum) . foldMap coerce

{-
  Ex 3.9

  note that this impl doesn't produce error when length mismatches.
 -}
fuse, fuseErr :: [Dur] -> [Dur -> Music a] -> [Music a]
fuse xs ys = getZipList $ flip ($) <$> ZipList xs <*> ZipList ys

fuseErr [] [] = []
fuseErr (x:xs) (y:ys) = y x : fuseErr xs ys
fuseErr _ _ = error "list length mismatches"

{- Ex 3.10 -}
{- HLINT ignore maxAbsPitchRec "Use foldl" -}
maxAbsPitchRec, maxAbsPitchNonRec :: [AbsPitch] -> AbsPitch
maxAbsPitchRec [] = error "empty list"
maxAbsPitchRec (x:xs) = maxAux x xs
  where
    maxAux nowMax [] = nowMax
    maxAux nowMax (y:ys) =
      maxAux (if y > nowMax then y else nowMax) ys

maxAbsPitchNonRec = maximum

{- Ex 3.11 -}
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = case compare a1 a2 of
    EQ -> note qn p1
    LT -> line1 $ note qn . pitch <$> [a1..a2]
    GT -> line1 $ note qn . pitch <$> [a1,a1-1..a2]
  where
    a1 = absPitch p1
    a2 = absPitch p2
