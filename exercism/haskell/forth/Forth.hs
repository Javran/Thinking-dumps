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

empty :: ForthState
empty = FState [] M.empty

evalProg :: forall r.
            ( Member (State ForthState) r
            , Member (Exc ForthError) r )
            => ForthCommand -> Eff r ()
evalProg fc = case fc of
    FNum v -> push v
    FDef name cmds -> do
        when (all isDigit name) (throwExc InvalidWord)
        modify (& fEnv %~ M.insert name cmds)
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
        "dup"  -> do { x <- pop; push x; push x }
        -- TODO: pattern match on stack for more efficient impl
        "swap" -> do { b <- pop; a <- pop; push b; push a }
        "drop" -> void pop
        "over" -> do { b <- pop; a <- pop; push a; push b; push a}
        _ -> throwExc (UnknownWord (T.pack cmd))
      where
        liftBinOp bin = do { b <- pop; a <- pop; push (a `bin` b) }
    evalWord :: String -> Eff r ()
    evalWord name = do
        env <- (^. fEnv) <$> get
        case M.lookup (map toLower name) env of
            Nothing -> throwExc (UnknownWord (T.pack name))
            Just cmds -> mapM_ evalProg cmds

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText rawText initState = run (runExc (execState initState (mapM evalProg (parseForthT rawText))))

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
