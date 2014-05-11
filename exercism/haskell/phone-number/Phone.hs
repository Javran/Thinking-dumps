module Phone
    ( areaCode
    , number
    , prettyPrint
    )
where

import Data.Char
import Data.Maybe
import Data.List.Split
import Text.Printf

-- | the normalized phone number considered as bad number
badNumber :: String
badNumber = replicate 10 '0'

-- | get normalized phone number from an arbitrary string
--   'badNumber' will be returned if the number given is bad.
number :: String -> String
number = fromMaybe badNumber . normalizeNum . pureNumber
    where
        -- filter non-digit chars
        pureNumber = filter isDigit
        -- normalize pure number
        normalizeNum s
              -- 10 digit phone num is good
            | len == 10 = Just s
              -- 11 digit phone num with leading @1@ is good.
            | len == 11 && head s == '1' = Just (tail s)
              -- otherwise, the number is bad
            | otherwise = Nothing
            where
                len = length s

-- | area code is the first 3 digits from a normalized phone number
areaCode :: String -> String
areaCode = take 3 . number

-- | normalize and pretty print a number
prettyPrint :: String -> String
prettyPrint xs = printf "(%s) %s-%s" area p1 p2
    where
        -- how we split a number into parts
        phoneSpec = [3,3,4] :: [Int]
        -- normalize
        nxs = number xs
        -- split by group
        (area:p1:p2:_) = splitPlaces phoneSpec nxs
