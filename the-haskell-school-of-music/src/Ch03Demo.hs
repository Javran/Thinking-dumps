{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch03Demo where

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

