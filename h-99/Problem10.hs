module Problem10 where

import Control.Arrow ((&&&))

import Problem09 (pack)

encode :: Eq a => [a] -> [(Int,a)]
encode = map (length &&& head) . pack

main :: IO ()
main = print (encode "aaaabccaadeeee")
