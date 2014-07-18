module Problem12 where

import Problem11 (RunLength(..))

decodeModified :: [RunLength a] -> [a]
decodeModified = concatMap decode
    where decode (Single x) = [x]
          decode (Multiple t x) = replicate t x

main :: IO ()
main = print . decodeModified $
    [ Multiple 4 'a', Single 'b', Multiple 2 'c'
    , Multiple 2 'a', Single 'd', Multiple 4 'e'
    ]
