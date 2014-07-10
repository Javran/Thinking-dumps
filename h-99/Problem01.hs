module Problem01 where

myLast :: [a] -> a
myLast [] = error "try to find an element on empty list"
myLast (x:[]) = x
myLast (_:xs) = myLast xs

main :: IO ()
main = do
    print $ myLast [1,2,3,4 :: Int]
    -- 4
    print $ myLast "xyz"
    -- 'z'
