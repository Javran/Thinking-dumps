module Problem21 where

type Sym = String

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n
    | n <= 0 || n > l = xs
    --  sure it's within range
    | n == 1 = x:xs
    | otherwise = let (h:t) = xs
                  in h:insertAt x t (n-1)
    where
        l = length xs

main :: IO ()
main = do
    print $ insertAt 'X' "abcd" 2
    print $ insertAt "alfa" (words "a b c d") 2
