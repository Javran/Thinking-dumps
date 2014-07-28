module Problem36 where

import Problem10 (encode)
import Problem35 (primeFactors)

swap :: (a,b) -> (b,a)
swap = uncurry (flip (,))

primeFactorsMult :: Int -> [(Int,Int)]
primeFactorsMult = map swap . encode . primeFactors

main :: IO ()
main = print $ primeFactorsMult 315
