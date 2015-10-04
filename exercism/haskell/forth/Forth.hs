{-# LANGUAGE OverloadedStrings, FlexibleContexts, TemplateHaskell, ScopedTypeVariables #-}
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
import qualified Data.Map as M
import Control.Lens
import Control.Eff
import Control.Eff.State.Strict
import Control.Eff.Exception

data ForthCommand
  = FNum Int -- number
  | FWord String -- primitives
  | FDef String [ForthCommand] -- definitions
  deriving Show

data ForthState = FState
  { _fStack :: [Int]
  , _fEnv :: M.Map String [ForthCommand]
  }

makeLenses ''ForthState

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord T.Text
  deriving (Show, Eq)

empty :: ForthState
empty = FState [] M.empty

evalProg :: forall r.
            ( Member (State ForthState) r
            , Member (Exc ForthError) r )
            => ForthCommand -> Eff r ()
evalProg fc = case fc of
    FNum v -> push v
    FDef name cmds -> modify (& fEnv %~ M.insert name cmds)
  where
    push v = modify (& fStack %~ (v:)) :: Eff r ()

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText = error "TODO: Evaluate an input Text, returning the new state"

formatStack :: ForthState -> T.Text
formatStack = T.pack . unwords . map show . reverse . (^. fStack)

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
                       else FWord raw)

-- the program consists of: printable but non-space chars
-- + all digits -> a number
-- + not all are digits -> a word (composed or primitive)
-- + otherwise its's a command (refered to by name)
definition :: ReadP ForthCommand
definition = do
    void $ lexeme (char ':')
    wordName <- lexeme (munch1 (not . isFSpace))
    as <- sepBy command skipFSpaces
    skipFSpaces
    void $ lexeme (char ';')
    let normWordName = map toLower wordName
    return (FDef normWordName as)

command :: ReadP ForthCommand
command = wordOrDigit +++ definition

parseForth :: String -> [ForthCommand]
parseForth = getResult . readP_to_S (sepBy command skipFSpaces
                                     <* skipFSpaces <* eof)
  where
    getResult xs = case filter (null . snd) xs of
        (x,_):_ -> x
        [] -> error "error while parsing"

parseForthT :: T.Text -> [ForthCommand]
parseForthT = parseForth . T.unpack
