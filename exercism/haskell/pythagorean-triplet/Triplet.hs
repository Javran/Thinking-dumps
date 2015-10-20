module Triplet
  ( mkTriplet
  , isPythagorean
  , pythagoreanTriplets
  ) where

import Data.List
import Control.Monad

type Triplet a = (a, a, a)

-- integer's square root, grab from Haskell wiki

squareRoot :: Integral a => a -> a
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
       (^!) :: Num a => a -> Int -> a
       (^!) x t = x^t
   in  head $ dropWhile (not . isRoot) iters

mkTriplet :: a -> a -> a -> Triplet a
mkTriplet = (,,)

isPythagorean :: Integral a => Triplet a -> Bool
isPythagorean (x,y,z) | [a,b,c] <- sort [x,y,z] = a*a + b*b == c*c
isPythagorean _ = error "impossible"

pythagoreanTriplets :: Integral a => a -> a -> [Triplet a]
pythagoreanTriplets from to = do
    (a:bs) <- tails [from .. to]
    b <- bs
    let sq = a*a + b*b
        sqr = squareRoot sq
    guard (sq == sqr * sqr && sqr <= to)
    return (mkTriplet a b sqr)
