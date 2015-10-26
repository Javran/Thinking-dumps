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
parseSgf t = case parse tree "" t of
    Left _ -> Nothing
    Right v -> Just v

{-# ANN node ("HLint: ignore Use string literal" :: String) #-}
node :: Parser SgfNode
node = char ';' >> M.fromList <$> many keyValue
  where
    value :: Parser T.Text
    value = between
              (char '[')
              (char ']')
              (T.pack . concat <$> many1 textChar)
      where
        textChar =
                (space >> return " ")
                -- if failed. it's not a space
                -- trivial cases
            <|> (:[]) <$> satisfy (\ch -> ch `notElem` [']','\\'])
                -- deal with escaping
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

tree :: Parser SgfTree
tree = between
         (char '(')
         (char ')')
         (mkTree <$> many1 node <*> many tree)
  where
    mkTree :: [SgfNode] -> [SgfTree] -> SgfTree
    mkTree nodes subtrees = case nodes of
        -- note that the first argument is received from "many1 p",
        -- it must be empty, we can also maintain this invariant on
        -- recursive calls
        [] -> error "impossible"
        [n] -> Node n subtrees
        (n:ns) -> Node n [mkTree ns subtrees]
