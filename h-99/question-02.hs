myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "invalid list"

main = do
    print $ myButLast [1,2,3,4]
    print $ myButLast ['a' .. 'z']
