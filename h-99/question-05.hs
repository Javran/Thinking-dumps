-- Problem 5:
-- reverse a list

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

main :: IO ()
main = print (myReverse [1 :: Int ..10])
