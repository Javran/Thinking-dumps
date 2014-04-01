elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
elementAt _ _ = error "invalid arguments"

main = do
    print $ elementAt [1,2,3] 2
    -- 2
    print $ elementAt "haskell" 5
    -- 'e'
