{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Forth
  ( ForthError(..)
  , ForthState
  , evalText
  , formatStack
  , empty
  ) where

import Text.ParserCombinators.ReadP hiding (get)
import Data.Char
import Data.Functor
import Control.Lens
import Control.Monad
import Control.Eff
import Control.Eff.State.Strict
import Control.Eff.Exception

import qualified Data.Text as T
import qualified Data.Map as M

data ForthCommand
  = FNum Int -- number
  | FWord String -- primitives -- TODO: when to normalize?
  | FDef String [ForthCommand] -- definitions

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

{-|
* Parsing
From testcases we know printable but non-space characters
are considered valid for this language.

Each consecutive non-space characters are considered a valid atom / token
for this language, additionally, there are few rules:

    * there are 2 special tokens: ":" is used to mark the beginning of
      a word definition whereas ";" marks the end of the definition
    * if a token consists of only digits, then it is a number
    * otherwise it's a word (referred to by name)

|-}


-- | return true if the character is non-printable or is space
isFSpace :: Char -> Bool
isFSpace x = not (isPrint x) || isSpace x

-- | skip spaces in Forth
skipFSpaces :: ReadP ()
skipFSpaces = void (munch isFSpace)

-- | transforms a parser so that it consumes all following spaces
--   in addition to finish its parsing task.
lexeme :: ReadP a -> ReadP a
lexeme = (<* skipFSpaces)

-- | parse a word or a digit, assuming the first character is not a space
wordOrDigit :: ReadP ForthCommand
wordOrDigit = do
    raw <- munch1 (not . isFSpace)
    case raw of
        -- ":" and ";" are special case, we shouldn't treat them
        -- as normal words
        ":" -> pfail
        ";" -> pfail
        _ -> return (if all isDigit raw
                       then FNum (read raw)
                       else FWord raw)

-- | parse a definition, assuming the first character is not a space
definition :: ReadP ForthCommand
definition = do
    void $ lexeme (char ':')
    wordName <- lexeme (munch1 (not . isFSpace))
    as <- sepBy command skipFSpaces
    skipFSpaces
    void $ lexeme (char ';')
    -- despite that names are case-insensitive
    -- we choose not to "normalize" it too early
    -- this could benefit error messages as less modification
    -- is introduced during parsing
    return (FDef wordName as)

-- | parse a forth command, a command is either word, digits or a definition
--   it is assumed the first character is not a space
command :: ReadP ForthCommand
command = wordOrDigit +++ definition

-- | parse a complete Forth program from raw strings
parseForth :: String -> [ForthCommand]
parseForth = getResult
           . readP_to_S (skipSpaces *>
                         sepBy command skipFSpaces
                         <* skipFSpaces <* eof)
  where
    getResult xs = case filter (null . snd) xs of
        (x,_):_ -> x
        [] -> error "error while parsing"

-- | parse a complete Forth program from raw texts
parseForthT :: T.Text -> [ForthCommand]
parseForthT = parseForth . T.unpack

empty :: ForthState
empty = FState [] M.empty

-- | evaluate a Forth program
evalProg :: forall r.
            ( Member (State ForthState) r
            , Member (Exc ForthError) r )
            => ForthCommand -> Eff r ()
evalProg fc = case fc of
    -- a number itself is a command of pushing that number
    FNum v -> push v
    -- for a definition, its name is first normalized by casting all characters
    -- into lowercases and store the definition in current environment
    FDef name cmds -> do
        let normName = map toLower name
        when (all isDigit name) (throwExc InvalidWord)
        modify (& fEnv %~ M.insert normName cmds)
    FWord name ->
        -- first try evalWord, if it fails, we fallback to attempt evalPrim
        -- if primitives are not expected to be overwritten, we can simply
        -- swap the position of evalWord and evalPrim, so primitives are attempted
        -- first without looking up the environment
        catchExc (evalWord name) $ \case
            UnknownWord n | name == T.unpack n -> evalPrim name
            e -> throwExc e
  where
    push :: Int -> Eff r ()
    push v = modify (& fStack %~ (v:))
    pop :: Eff r Int
    pop = ((^. fStack) <$> get) >>=
          \case
              [] -> throwExc StackUnderflow
              (v:xs) -> modify (& fStack .~ xs) >> return v
    -- evaluate primitive operations
    evalPrim :: String -> Eff r ()
    evalPrim cmd = case map toLower cmd of
        "+" -> liftBinOp (+)
        "-" -> liftBinOp (-)
        "*" -> liftBinOp (*)
        "/" -> do
            b <- pop
            when (b == 0) (throwExc DivisionByZero)
            a <- pop
            push (a `div` b)
        -- DUP, SWAP, DROP, OVER can be implemented in terms
        -- of push and pop operations,
        -- but here we directly manipulate the whole stack instead
        -- it's just a trade-off between efficiency and simplicity
        "dup"  -> do
            stk <- (^. fStack) <$> get
            case stk of
                (x:_) -> modify (& fStack .~ (x:stk))
                _ -> throwExc StackUnderflow
        "swap" -> do
            stk <- (^. fStack) <$> get
            case stk of
                (a:b:xs) -> modify (& fStack .~ (b:a:xs))
                _ -> throwExc StackUnderflow
        "drop" -> void pop
        "over" -> do
            stk <- (^. fStack) <$> get
            case stk of
                (_:b:_) -> modify (& fStack .~ (b:stk))
                _ -> throwExc StackUnderflow
        _ -> throwExc (UnknownWord (T.pack cmd))
      where
        liftBinOp bin = do { b <- pop; a <- pop; push (a `bin` b) }
    -- evaluate a word by looking up the environment to pick
    -- the correponding sequence of commands
    evalWord :: String -> Eff r ()
    evalWord name = do
        env <- (^. fEnv) <$> get
        case M.lookup (map toLower name) env of
            Nothing -> throwExc (UnknownWord (T.pack name))
            Just cmds -> mapM_ evalProg cmds

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText rawText initState =
      run
    . runExc -- handle exception requests
    . execState initState -- handle state requests
    $ program
  where
    ast = parseForthT rawText
    program = mapM evalProg ast

formatStack :: ForthState -> T.Text
formatStack = T.pack . unwords . map show . reverse . (^. fStack)
