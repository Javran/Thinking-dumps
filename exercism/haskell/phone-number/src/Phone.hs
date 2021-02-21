{-# LANGUAGE ViewPatterns #-}

module Phone
  ( number
  )
where

import Data.Char
import Text.ParserCombinators.ReadP

nanpNum :: ReadP String
nanpNum = do
  let nd = satisfy (>= '2')
      xd = get
  optional (char '1')
  sequence $ [nd, xd, xd, nd, xd, xd] <> replicate 4 xd

-- Get normalized phone number from an arbitrary string
-- 'badNumber' will be returned if the number given is bad.
number :: String -> Maybe String
number (filter isDigit -> xs) = do
  [(s, "")] <- pure $ readP_to_S (nanpNum <* eof) xs
  pure s
