module Problem13 where

import Problem11 (RunLength(..), freqPairToRunLen)

encodeDirect :: Eq a => [a] -> [RunLength a]
encodeDirect = map freqPairToRunLen . encode
    where
        encode :: Eq a => [a] -> [(Int,a)]
        encode = reverse . foldl go []
            where go [] i = [(1,i)]
                  go ((fq,e):xs) i =
                      if i == e
                         then (fq+1,e):xs
                         else (1,i):(fq,e):xs

main :: IO ()
main = print . encodeDirect $ "aaaabccaadeeee"
