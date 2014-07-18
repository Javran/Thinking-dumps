module Problem18 where

import Problem17 (split)

slice :: [a] -> Int -> Int -> [a]
slice xs i j = let (_,bs) = split xs (i-1)
                   (ans,_) = split bs (j-i+1)
               in ans

main :: IO ()
main = print $ slice "abcdefghik" 3 7
