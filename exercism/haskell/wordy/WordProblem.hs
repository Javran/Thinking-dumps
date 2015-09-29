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

operation :: ReadP (Integer -> Integer)
operation = do
    let parseOp = (string "plus" *> return (+))
              +++ (string "minus" *> return (-))
              +++ (string "multiplied by" *> return (*))
              +++ (string "divided by" *> return div)
    op <- skipSpaces *> parseOp <* skipSpaces
    n <- number
    return (`op` n)

-- patterns:
-- number : (-?)(\d+)
-- operations: "plus", "minus", "multipled by", "divided by"
