module Brackets
  ( arePaired
  )
where

import Text.ParserCombinators.ReadP

lang :: ReadP ()
lang = skipComments *> skipMany paired <* eof
  where
    -- everything other than brackets are "comments"
    skipComments = munch (`notElem` "()[]{}")
    lm = (<* skipComments)
    chToken ch = lm (char ch)
    paired = do
      let lrParen l r =
            -- consumes <l> and expect closing token to be <r>.
            r <$ chToken l
      rParen <-
        lrParen '(' ')'
          <++ lrParen '[' ']'
          <++ lrParen '{' '}'
      skipMany paired
      chToken rParen

arePaired :: String -> Bool
arePaired xs = case readP_to_S lang xs of
  [((), "")] -> True
  _ -> False
