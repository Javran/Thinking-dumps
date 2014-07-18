module Problem15 where

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

repli :: [a] -> Int -> [a]
repli xs t = concatMap (replicate t) xs

main :: IO ()
main = print $ repli "abc" 3
