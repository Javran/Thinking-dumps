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

-- some extra rules not mentioned but found in the testcases:
-- - keys should be all-uppercases

-- the following testcase is not making any sense to me:
--
--   , tps "(;A[\\]b\nc\\\nd\t\te\\\\ \\\n\\]])" $
--    Just $ tn [("A", ["]b cd  e\\ ]"])]

-- '(;A[\]b<newline>c\<newline>d  e\\ \<newline> \]])'
-- well I need to read the doc: http://www.red-bean.com/sgf/sgf4.html#text

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
