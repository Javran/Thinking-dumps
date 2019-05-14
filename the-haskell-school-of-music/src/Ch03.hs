{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Ch03 where

import Data.Bits
import Data.Coerce
import Data.Monoid
import Euterpea

import qualified Ch02

{- Ex 3.1 -}
f1 :: Int -> [Pitch] -> [Pitch]
f1 = Ch02.transM

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
fuse = zipWith (flip ($))

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

{-
   Ex 3.11

   I'm skipping the recursive definition
   as it'll get verbose and less efficient unless proper bookkeeping is done
   (so that we don't need to compare every single recursive call)

 -}
chrom :: Pitch -> Pitch -> Music Pitch
chrom p1 p2 = case compare a1 a2 of
    EQ -> note qn p1
    LT -> line1 $ note qn . pitch <$> [a1..a2]
    GT -> line1 $ note qn . pitch <$> [a1,a1-1..a2]
  where
    a1 = absPitch p1
    a2 = absPitch p2

{- Ex 3.12 -}
mkScale :: Pitch -> [Int] -> Music Pitch
mkScale p = line1 . (note qn <$>) . scanl (\a i -> pitch $ absPitch a + i) p

{- Ex 3.13 -}
data MyScale
  = MyIonian
  | MyDorian
  | MyPhrygian
  | MyLydian
  | MyMixolydian
  | MyAeolian
  | MyLocrian

genScale :: MyScale -> Pitch -> Music Pitch
genScale s p = mkScale p $ case s of
  MyIonian -> [2,2,1,2,2,2,1]
  MyDorian -> [2,1,2,2,2,1,2]
  MyPhrygian -> [1,2,2,2,1,2,2]
  MyLydian -> [2,2,2,1,2,2,1]
  MyMixolydian -> [2,2,1,2,2,1,2]
  MyAeolian -> [2,1,2,2,1,2,2]
  MyLocrian -> [1,2,2,1,2,2,2]

{- Ex 3.14: TODO -}
frereJacques :: Music Pitch
frereJacques = l1 :+: l1 :+: l2 :+: l2 :+: l3 :+: l3 :+: l4 :+: l4
  where
    l1 = line1 (map n [(G,4),(A,4),(B,4),(G,4)])
    l2 = line1 (map n [(B,4),(C,5),(D,5)]) :+: rest qn
    l3 = line1 (map e [(D,5),(E,5),(D,5),(C,5)]) :+: line1 (map n [(B,4),(G,4)])
    l4 = line1 (map n [(G,4),(D,4),(G,4)]) :+: rest qn
    e = note en
    n = note qn

{- Ex 3.15 -}
encrypt :: Char -> Int
encrypt = (0xFF .&.) . succ . fromEnum

decrypt :: Int -> Char
decrypt = toEnum . (0xFF .&.) . (+ 255)

checkEncryptDecrypt :: Bool
checkEncryptDecrypt = and $
  [x == x' | x <- toEnum <$> [0..255], let x' = decrypt . encrypt $ x]
  <> [y == y'| y <- [0..255], let y' = encrypt . decrypt $ y]
