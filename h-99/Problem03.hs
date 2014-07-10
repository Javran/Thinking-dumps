module Problem03 where

elementAt :: [a] -> Int -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)
elementAt _ _ = error "invalid arguments"

main :: IO ()
main = do
    print $ elementAt [1,2,3 :: Int] 2
    -- 2
    print $ elementAt "haskell" 5
    -- 'e'
