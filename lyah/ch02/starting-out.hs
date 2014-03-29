-- baby's first function
doubleMe x = x + x

doubleUs1 x y = x*2 + y*2

doubleUs2 x y = doubleMe x + doubleMe y

doubleUs3 x y = sum $ map doubleMe [x,y]

-- `if` is an expression, and `else` is mandatory
doubleSmallNumber x = if x > 100 then x else x*2

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1

{-# ANN conanO'Brien "HLint: ignore" #-}
-- `'` is nothing special
conanO'Brien = "It's a-me, Conan O'Brien"

main = do
    print $ take 10 $ iterate succ 1
    print $ min 1 2
    print $ max 3 4
    -- function application has the highest precedence
    print $ succ 9 + max 5 4 + 1
    -- 10 + 5 + 1 -> 16
    print $ succ 9 * 10
    print $ succ $ 9 * 10
    -- call as infix function
    print $ mod 100 37
    print $ 100 `mod` 37
    putStrLn $ replicate 10 '-'
    print $ map ((`map` [(1,2), (3,4), (5,6)]).uncurry) [doubleUs1, doubleUs2, doubleUs3]
    print $ map doubleSmallNumber [0,10..200]
