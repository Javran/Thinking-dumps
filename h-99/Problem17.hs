module Problem17 where

split :: [a] -> Int -> ([a],[a])
split xs n
    | n <= 0 = ([],xs)
    | otherwise =
        case split xs (n-1) of
          (as,b:bs) -> (as ++ [b], bs)
          (as,[]) -> (as,[])

main :: IO ()
main = print $ split "abcdefghik" 3
