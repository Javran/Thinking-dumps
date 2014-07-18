module Problem22 where

range :: Int -> Int -> [Int]
range i j = case i `compare` j of
              LT -> i : range (i+1) j
              EQ -> [i]
              GT -> []

main :: IO ()
main = print $ range 4 9
