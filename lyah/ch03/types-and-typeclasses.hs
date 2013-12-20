removeNonUppercase :: String -> String
removeNonUppercase st =
    [ c | c <- st
        , c `elem` ['A'..'Z']]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

main = do
    putStrLn "Types"
    print ('a'         :: Char)
    print (True        :: Bool)
    print ("HELLO"     :: String)
    print ((True, 'a') :: (Bool, Char))
    print (4 == 5      :: Bool)
    putStrLn "Functions"
    print $ (removeNonUppercase :: String -> String) "AbCdE"
    print $ addThree 1 2 3
