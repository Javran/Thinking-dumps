{-# LANGUAGE OverloadedStrings, MultiWayIf #-}
module Sgf
  ( parseSgf
  ) where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Tree
import Data.Char
import Text.Parsec
import Text.Parsec.Text
import Data.Functor

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

{-# ANN node ("HLint: ignore Use string literal" :: String) #-}
node :: Parser SgfNode
node = char ';' >> M.fromList <$> many1 keyValue
  where
    value :: Parser T.Text
    value = between
              (char '[')
              (char ']')
              (T.pack . concat <$> many1 textChar)
      where
        -- TODO: return string, because "\\\n" should produce no output
        -- parsing to Char is not capable of doing this.
        textChar =
                -- if failed. it's not a space
                (many1 space >> return " ")
                -- trivial cases
            <|> (:[]) <$> satisfy (\ch -> ch `notElem` [']','\\'])
            <|> do
                    void (char '\\')
                    ch <- anyChar
                    if | ch == '\n' -> return ""
                       | isSpace ch -> return " "
                       | otherwise -> return [ch]

    -- one key with at least one value
    keyValue :: Parser (T.Text, [T.Text])
    keyValue = do
        key <- T.pack <$> many1 (satisfy isUpper)
        vs <- many1 value
        return (key, vs)

testF :: String -> IO ()
testF raw = parseTest node (T.pack raw)
