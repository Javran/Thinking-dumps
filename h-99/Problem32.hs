module Problem32 where

myGCD :: Int -> Int -> Int
myGCD a 0 = abs a
myGCD a b = myGCD b (a `mod` b)

main :: IO ()
main = print $ map (uncurry myGCD) [ (36,63), (-3,-6),(-3,6) ]
