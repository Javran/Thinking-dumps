module IsbnVerifier
  ( isbn
  )
where

import Control.Monad
import Data.Char
import Text.ParserCombinators.ReadP

isbnP :: ReadP [Int]
isbnP = do
  _ <- spc
  xs <- replicateM 9 (lm digit)
  y <- lm checkDigit
  pure $ xs <> [y]
  where
    spc = void (munch (== '-'))
    lm = (<* spc)
    digit = (\ch -> ord ch - ord '0') <$> satisfy isDigit
    checkDigit = (10 <$ char 'X') <++ digit

isbn :: String -> Bool
isbn xs = case readP_to_S (isbnP <* eof) xs of
  [(ds, "")] ->
    sum (zipWith (*) ds [10, 9 ..]) `rem` 11 == 0
  _ -> False
