-- Problem 5:
-- reverse a list

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

{-
example: [1,2,3,4]
=> 1:2:3:4:[] ([])
=> 2:3:4:[] (1:[])
=> 3:4:[] (2:1:[])
=> 4:[] (3:2:1:[])
=> [] (4:3:2:1:[])

-}

-- drawback: (++) and (:[]) are costly
myReverse1 :: [a] -> [a]
myReverse1 [] = []
myReverse1 (x:xs) = myReverse1 xs ++ [x]

-- avoids (++) and (:[]) totally, but needs `init` and `last'
myReverse2 :: [a] -> [a]
myReverse2 xs
    | null xs = []
    | otherwise = last xs : myReverse2 (init xs)

{-
  last xs : myReverse2 (init xs)
> (flip (:)) (myReverse2 (init xs)) (last xs)
> (flip (:) (myReverse2 ys) y  -- let (ys,y) = (init &&& last) xs
> (flip (:) reversed y)        -- let reversed = myReverse2 ys
-}

main :: IO ()
main = do
    let d :: [Int]
        d = [1..10]

    print $ myReverse d
    print $ myReverse1 d
    print $ myReverse2 d
