module Problem37 where

import Problem34 (totient)
import Problem36 (primeFactorsMult)

phi :: Int -> Int
phi = product . map (\(p,m) -> (p-1)*p^(m-1)) . primeFactorsMult

main :: IO ()
main = do
    let xs = [1 :: Int ..50]
        as = map totient xs
        bs = map phi xs
    print as
    print bs
    print $ and $ zipWith (==) as bs
