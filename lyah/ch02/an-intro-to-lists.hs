main = do
    let lostNumbers = [4,8,15,16,23,42]
    print lostNumbers
    -- put two lists together: (++)
    print $ [1,2,3,4] ++ [9,10,11,12]
    print $ "l" ++ "y" ++ "a" ++ "h"
    -- put something at beginning: (:)
    print $ 'a' : "bcde"
    -- syntactic sugar.
    print $ 1:2:3:4:[] == [1,2,3,4]
    -- get an element: (!!)
    print $ [1,2,3,4] !! 2
    let b = [[1,2,3], [], [4,5,6]]
    print b
    -- list comparison
    print $ [1,2,3] < [1,2,4]

    putStrLn "> List operations"
    -- head : [tail] = <a list>
    print $ head lostNumbers
    print $ tail lostNumbers
    -- [init] : last = <a list>
    print $ init lostNumbers
    print $ last lostNumbers
    -- length & null
    print $ length lostNumbers
    print $ null []
    print $ null [1]

    -- take & drop
    print $ take 3 lostNumbers
    print $ drop 3 lostNumbers
    print $ splitAt 3 lostNumbers

    -- maximum & minimum
    print $ maximum lostNumbers
    print $ minimum lostNumbers

    -- sum & product
    let nums = [1..10]
    print $ sum nums
    print $ product nums
    
    -- membership: elem
    print $ 3 `elem` nums
    print $ 11 `elem` nums

    putStrLn "> Ranges"
    print $ [1..20]
    print $ ['a'..'z']
    print $ [10,9..1]
    -- watch out floating numbers..
    print $ [0.1,0.3..1]

    print $ take 10 $ cycle [1,2,3]
    print $ take 10 $ repeat 5
    print $ replicate 10 6

    putStrLn "> List comprehension"
    print $ [x*2 | x <-[1..10]]
    print $ [x | x <- [50..100], x `mod` 7 == 3]

    -- I don't care: (_)
    let length' xs = sum [1 | _ <- xs ]
    print $ length $ replicate 10 1

