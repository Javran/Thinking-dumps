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
  | FDef [ForthCommand] -- definitions

empty :: ForthState
empty = error "TODO: An empty ForthState"

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText = error "TODO: Evaluate an input Text, returning the new state"

formatStack :: ForthState -> T.Text
formatStack = error "TODO: Return the current stack as Text with the element \
                    \on top of the stack being the rightmost element in the \
                    \output"

-- the program consists of: printable but non-space chars
-- + all digits -> a number
-- + not all are digits -> a word (composed or primitive)
-- + otherwise its's a command (refered to by name)
atom :: ReadP ForthCommand
atom = do
    void (munch (not . isPrint))
    raw <- munch1 (\x -> isPrint x && not (isSpace x))
    -- TODO: check if this is a definition
    if all isDigit raw
      then return (FNum (read raw))
      else return (FPrim raw)
