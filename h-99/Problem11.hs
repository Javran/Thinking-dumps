module Problem11 where

import Problem10 (encode)

data RunLength a
    = Single a
    | Multiple Int a
      deriving (Show)

encodeModified :: Eq a => [a] -> [RunLength a]
encodeModified = map convert . encode
    where
        convert :: (Int,a) -> RunLength a
        convert (time,x) = case time of
                               1 -> Single x
                               _ -> Multiple time x

main :: IO ()
main = print . encodeModified $ "aaaabccaadeeee"
