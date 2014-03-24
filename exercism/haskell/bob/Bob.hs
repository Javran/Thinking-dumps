module Bob
where

import Control.Arrow
import Data.Char
import Data.Maybe

safeLast :: [a] -> Maybe a
safeLast = listToMaybe . reverse

isQuestion :: String -> Bool
isQuestion = safeLast >>>
             maybe
                 -- empty list
                 False
                 -- a question mark at the end
                 (== '?')

-- | the string has some alphas, and all alphas are uppers
isYell :: String -> Bool
                              -- <seen uppers?> <keep going?>
isYell xs = foldr go (const True) xs False
    where
        go i acc seenUpper
           | isUpper i = acc True
           | isAlpha i && isLower i = False
           | not (isAlpha i) = acc seenUpper

responseFor :: String -> String
responseFor = responseFor' . stripAll
    where
        stripL = dropWhile isSpace
        stripR = reverse . stripL . reverse
        stripAll = stripR . stripL

responseFor' :: String -> String
responseFor' s
    | null s       = "Fine. Be that way!"
    | isYell s     = "Woah, chill out!"
    | isQuestion s = "Sure."
    | otherwise    = "Whatever."

-- Local variables:
-- proc-entry: "bob_test.hs"
-- End:
