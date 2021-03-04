module WordProblem
  ( answer
  )
where

import Text.ParserCombinators.ReadP

-- | reads operation (consumes leading and following consecutive spaces)
--   and produce the corresponding operation
operation :: ReadP (Integer -> Integer -> Integer)
operation = skipSpaces *> op <* skipSpaces
  where
    op =
      ((+) <$ string "plus")
        <++ ((-) <$ string "minus")
        <++ ((*) <$ string "multiplied by")
        <++ (div <$ string "divided by")

answer :: String -> Maybe Integer
answer inp = do
  [(v, "")] <- pure $ readP_to_S (question <* eof) inp
  pure v
  where
    question :: ReadP Integer
    question =
      string "What is "
        *> chainl1 (readS_to_P reads) operation <* char '?'
