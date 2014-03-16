import Control.Monad (guard)

-- here is an example demonstrating
-- the similarity between list monad and amb.
-- compare this function with the one in exercise 4.35

aPythagoreanTripleBetween :: Int -> Int -> [(Int, Int, Int)]
aPythagoreanTripleBetween low high = do
    i <- [low..high]
    let hsq = high * high
    j <- [i..high]
    let ksq = i * i + j * j
    guard (hsq >= ksq)
    let k = sqrt (fromIntegral ksq) :: Float
    guard (k == fromIntegral (round k:: Int))
    return (i,j,round k)

main :: IO ()
main = print (aPythagoreanTripleBetween 1 100)
