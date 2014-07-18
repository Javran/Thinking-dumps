module Problem02 where

myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "invalid list"

main :: IO ()
main = do
    print $ myButLast [1,2,3,4 :: Int]
    print $ myButLast ['a' .. 'z']
