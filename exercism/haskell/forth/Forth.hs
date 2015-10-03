{-# LANGUAGE OverloadedStrings #-}
module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import qualified Data.Text as T
import Text.ParserCombinators.ReadP hiding (skipSpaces)
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

-- | skip spaces in Forth
skipSpaces :: ReadP ()
skipSpaces = void (munch (\x -> not (isPrint x) || isSpace x))

lexeme :: ReadP a -> ReadP a
lexeme = (<* skipSpaces)

-- the program consists of: printable but non-space chars
-- + all digits -> a number
-- + not all are digits -> a word (composed or primitive)
-- + otherwise its's a command (refered to by name)
atom :: ReadP ForthCommand
atom = do
    raw <- munch1 (\x -> isPrint x && not (isSpace x))
    case raw of
        ":" -> do
            skipSpaces
            wordName <- lexeme (munch1 (\x -> isPrint x && not (isSpace x)))
            traceM wordName
            as <- sepBy ((char ';' >> return (FDef "" [])) <++ atom) skipSpaces
            char ';' >> skipSpaces
            return (FDef wordName as)
        _ -> return (if all isDigit raw
                       then FNum (read raw)
                       else FPrim raw)
