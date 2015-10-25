{-# LANGUAGE OverloadedStrings #-}
module Sgf
  ( parseSgf
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Tree
import Data.Char
import Text.Parsec
import Text.Parsec.Text

type SgfNode = M.Map T.Text [T.Text]
type SgfTree = Tree SgfNode

{-

To summarize, we need to take care of all the following details:

* a key name consists of only uppercase letters
* at least one value for each key name, could be more than one
* whitespaces otherthan linebreaks are converted to space
* soft line break are those linebreaks proceded by a "\"
* soft line breaks are converted to spaces
* hard link break "\n" should be kept as-is
* escaping: "\<any>" is just "<any>", when "<any>" is not whitespaces
  (note that linebreaks are also whitespaces, but we need to deal with it
   differently)

-}

parseSgf :: T.Text -> Maybe SgfTree
parseSgf = undefined

node :: Parser SgfNode
node = char ';' >> M.fromList <$> many1 keyValue
  where
    value :: Parser T.Text
    value = between
              (char '[')
              (char ']')
              _
    -- one key with at least one value
    keyValue :: Parser (T.Text, [T.Text])
    keyValue = do
        key <- T.pack <$> many1 (satisfy isUpper)
        vs <- many1 value
        return (key, vs)
