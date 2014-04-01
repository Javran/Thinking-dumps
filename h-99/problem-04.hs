myLength :: [a] -> Int
myLength xs = foldl1 (+) $ map (const 1) xs

main = do
    print $ myLength [123, 456, 789]
    -- 3
    print $ myLength "Hello, world!"
    -- 13
