module Problem96 where

import Text.ParserCombinators.ReadP hiding (many)
import Data.Char
import Data.Maybe
import Control.Applicative

identifierP :: ReadP String
identifierP = (:) <$> letter <*> (concat <$> many followed)
  where
    letter = satisfy isLetter
    digit = satisfy isDigit
    neg = char '-'

    followed = do
        n <- option Nothing (Just <$> neg)
        x <- letter <|> digit
        pure $ maybeToList n ++ [x]

identifier :: String -> Bool
identifier = not . null . readP_to_S (identifierP <* eof)
