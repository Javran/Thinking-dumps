module Problem04 where

myLength :: [a] -> Int
myLength = sum . map (const 1)

main :: IO ()
main = do
    print $ myLength [123, 456, 789 :: Int]
    -- 3
    print $ myLength "Hello, world!"
    -- 13
