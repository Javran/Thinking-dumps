myLast :: [a] -> a
myLast [] = error "try to find an element on empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

main = do
    print $ myLast [1,2,3,4]
    -- 4
    print $ myLast ['x','y','z']
    -- 'z'
