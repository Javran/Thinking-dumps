module Problem19 where

rotate :: [a] -> Int -> [a]
rotate xs n = take l . drop n' . cycle $ xs
    where l = length xs
          n' = ((n `mod` l) + l) `mod` l

main :: IO ()
main = do
    print $ rotate "abcdefgh" 3
    print $ rotate "abcdefgh" (-2)
