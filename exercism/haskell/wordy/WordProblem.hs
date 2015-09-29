module WordProblem
  ( answer
  ) where

import Text.ParserCombinators.ReadP
import Data.Char

answer :: String -> Int
answer = undefined

number :: ReadP Integer
number = do
    sgnF <- option id (char '-' *> return negate)
    sgnF . read <$> munch1 isDigit

operation :: ReadP (Integer -> Integer -> Integer)
operation = skipSpaces *> op <* skipSpaces
  where
    op = (string "plus" *> return (+))
     +++ (string "minus" *> return (-))
     +++ (string "multiplied by" *> return (*))
     +++ (string "divided by" *> return div)

question :: ReadP Integer
question = do
    string "What is "
    result <- chainl1 number operation
    char '?'
    return result

-- patterns:
-- number : (-?)(\d+)
-- operations: "plus", "minus", "multipled by", "divided by"
