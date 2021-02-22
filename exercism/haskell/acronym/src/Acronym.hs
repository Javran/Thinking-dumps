module Acronym
  ( abbreviate
  )
where

import Data.Char
import Text.ParserCombinators.ReadP

{-
  Garbage test suite with random made up rules in, garbage impl out.

  Complains aside, we need to separate words, this seems to work:

  - <uppercase>, followed by all uppercases or lowercases, and '\'', is a word.
  - <lowercase>, followed by all lowercase, is a word.
  - drop anything else.
 -}

lang :: ReadP [(Char, String)]
lang = word `sepBy` munch (not . isAlpha)
  where
    {-
      returns first character in uppercase
      and the full word as-is (not necessary, just in case this needs debugging).
     -}
    word = do
      hd <- satisfy isAlpha
      let lowers1 = munch1 (\ch -> isLower ch || ch == '\'')
          hd' = toUpper hd
      if isUpper hd
        then do
          tl <-
            munch1 (\ch -> isUpper ch || ch == '\'')
              <++ lowers1
              <++ pure []
          pure (hd', hd : tl)
        else do
          tl <-
            lowers1
              <++ pure []
          pure (hd', hd : tl)

abbreviate :: String -> String
abbreviate xs = case readP_to_S (lang <* eof) xs of
  [(vs, "")] -> fmap fst vs
  _ -> error "parse error"
