module Problem16 where

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = concat
               $ zipWith (\i x -> [x | i])
                         [ y `mod` n /= 0 | y <- [1..] ]
                         xs

main :: IO ()
main = print $ dropEvery "abcdefghik" 3
