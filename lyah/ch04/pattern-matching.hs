
-- pattern matching
lucky :: (Integral a) => a -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky _ = "Sorry, you're out of luck, pal!"

sayMe :: (Integral a) => a -> String
sayMe 1 = "One!"
sayMe 2 = "Two!"
sayMe 3 = "Three!"
sayMe 4 = "Four!"
sayMe 5 = "Five!"
sayMe _ = "Not between 1 and 5"

-- recursive function
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- pattern matching might fail
charName :: Char -> String
-- will have a warning here complaining
-- pattern matches are not exhaustive
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

main :: IO ()
main = do
     print $ lucky (7 :: Int)
     print $ lucky (8 :: Int)
     print $ sayMe (2 :: Int)
     print $ factorial (10 :: Int)
     print $ charName 'a'
     mapM_ (print . tell) [[],[1 :: Int],[1,2],[1,2,3]]
