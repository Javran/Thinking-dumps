module Bob
where

import Control.Arrow
import Data.Char
import Data.Maybe

safeLast :: [a] -> Maybe a
safeLast = listToMaybe . reverse

-- | have question mark at the end
isQuestion :: String -> Bool
isQuestion = safeLast >>>
             maybe
                 -- empty list
                 False
                 -- a question mark at the end
                 (== '?')

-- | contain some alpha, and all alphas are uppers
isYell :: String -> Bool
isYell = (any isAlpha
         -- ^ at least contains some alphas
         &&& (filter isAlpha >>> all isUpper))
         -- ^ all alphas being uppers
         >>> uncurry (&&)
         -- ^ both conds are required

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
