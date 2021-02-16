module WordProblem
  ( answer
  ) where

import Text.ParserCombinators.ReadP
import Data.Char
import Data.Maybe

answer :: String -> Maybe Integer
answer = parse question

number :: ReadP Integer
number = do
    -- an optional sign is parsed as a function that
    -- transforms the number immediately followed
    sgnF <- option id (char '-' *> return negate)
    sgnF . read <$> munch1 isDigit

-- | reads operation (consumes leading and following consecutive spaces)
--   and produce the corresponding operation
operation :: ReadP (Integer -> Integer -> Integer)
operation = skipSpaces *> op <* skipSpaces
  where
    op = (string "plus" *> return (+))
     +++ (string "minus" *> return (-))
     +++ (string "multiplied by" *> return (*))
     +++ (string "divided by" *> return div)

-- | parse the question, calculate results as parser runs
question :: ReadP Integer
question = string "What is "
        *> chainl1 number operation <* char '?'

parse :: ReadP a -> String -> Maybe a
parse rp inp = listToMaybe
             . map fst
             . filter (null . snd)
             $ readP_to_S (rp <* eof) inp
