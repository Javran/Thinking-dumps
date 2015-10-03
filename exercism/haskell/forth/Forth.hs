{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import qualified Data.Text as T
import Text.ParserCombinators.ReadP
import Data.Char
import Data.Functor
import Debug.Trace

data ForthState -- TODO: define this data type

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord T.Text
  deriving (Show, Eq)

data ForthCommand
  = FNum Int -- number
  | FPrim String -- primitives
  | FDef String [ForthCommand] -- definitions
    deriving Show

empty :: ForthState
empty = error "TODO: An empty ForthState"

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText = error "TODO: Evaluate an input Text, returning the new state"

formatStack :: ForthState -> T.Text
formatStack = error "TODO: Return the current stack as Text with the element \
                    \on top of the stack being the rightmost element in the \
                    \output"

-- non-printables count as spaces
isFSpace :: Char -> Bool
isFSpace x = not (isPrint x) || isSpace x

-- | skip spaces in Forth
skipFSpaces :: ReadP ()
skipFSpaces = void (munch isFSpace)

lexeme :: ReadP a -> ReadP a
lexeme = (<* skipFSpaces)

-- | parse a word or a digit
wordOrDigit :: ReadP ForthCommand
wordOrDigit = do
    raw <- munch1 (not . isFSpace)
    case raw of
        ":" -> pfail
        ";" -> pfail
        _ -> return (if all isDigit raw
                       then FNum (read raw)
                       else FPrim raw)

-- the program consists of: printable but non-space chars
-- + all digits -> a number
-- + not all are digits -> a word (composed or primitive)
-- + otherwise its's a command (refered to by name)
definition :: ReadP ForthCommand
definition = do
    void $ lexeme (char ':')
    wordName <- lexeme (munch1 (not . isFSpace))
    traceM wordName
    as <- sepBy command skipFSpaces
    skipFSpaces
    void $ lexeme (char ';')
    return (FDef wordName as)

command :: ReadP ForthCommand
command = wordOrDigit +++ definition
