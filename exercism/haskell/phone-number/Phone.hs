module Phone
    ( areaCode
    , number
    , prettyPrint
    )
where

import Data.Char
import Data.Maybe
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
              -- 11 digit phone num is
            | len == 11 && head s == '1' = Just (tail s)
              -- length should be either 11 or 10 digits
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
        -- normalize
        nxs = number xs
        -- split by group
        (area:p1:p2:_) = splitByGroup [3,3,4] nxs

-- | split a list according to the first list given.
--   @splitByGroup [a1,a2]@ is the same as
--   @splitAt a1@ to fetch the first sublist,
--   and call @splitAt a2@ on the rest of its result.
splitByGroup :: [Int] -> [a] -> [[a]]
splitByGroup [] xs = [xs]
splitByGroup (l:ls) xs = a1 : splitByGroup ls a2
    where (a1,a2) = splitAt l xs
