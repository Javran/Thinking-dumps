{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Forth
  ( ForthError (..)
  , ForthState
  , evalText
  , toList
  , emptyState
  , parseForth
  )
where

import Control.Applicative
import Control.Carrier.Error.Either
import Control.Carrier.State.Strict
import Control.Monad
import qualified Data.Attoparsec.Text as P
import Data.Bifunctor
import Data.Char
import qualified Data.Map as M
import qualified Data.Text as T

data ForthError
  = DivisionByZero
  | StackUnderflow
  | InvalidWord
  | UnknownWord T.Text
  | SyntaxError String
  deriving (Show, Eq)

type ForthState = (Env, [Int])

data Stmt
  = -- | Statement that pushs a number
    Num Int
  | -- | Statement that invokes a symbol
    Sym T.Text
  | -- | Word declaration, name must be in lowercase.
    WordDecl T.Text [Stmt]
  deriving (Show)

type Env = M.Map T.Text Action

data Action
  = -- | A primitive action operates on ForthState and might raise failure through Either
    Prim (ForthState -> Either ForthError ForthState)
  | -- | A user-defined word with environment when this word is defined.
    Closure Env [Stmt]

emptyState :: ForthState
emptyState = (initEnv, [])

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText text st =
  case parseForth text of
    Left attoErr -> Left $ SyntaxError attoErr
    Right stmts ->
      bimap id fst
        . run
        . runError
        . runState st
        $ mapM_ evalStmt stmts

toList :: ForthState -> [Int]
toList = reverse . snd

evalAction :: (Has (State ForthState) sig m, Has (Error ForthError) sig m) => Action -> m ()
evalAction = \case
  Prim prim -> do
    s <- get
    case prim s of
      Left err -> throwError err
      Right s' -> put s'
  Closure cEnv body -> do
    {-
      We have a design decision to make here about whether to
      take this potentially updated cEnv and write that back to
      this current closure, which might require we to establish identity
      for each created closure (which might mean to maintain more things in ForthState,
      including a global closure counter). Won't go that extra mile though,
      as test cases never involve topics about defining words inside definitions.
     -}
    fEnv <- gets @ForthState fst
    let restore = modify @ForthState (first (const fEnv))
    modify @ForthState (first (const cEnv))
    -- make sure to recover environment otherwise we are exiting with closure's environment set.
    mapM_ evalStmt body
      `catchError` (\e -> restore >> throwError @ForthError e)
    restore

evalStmt :: (Has (State ForthState) sig m, Has (Error ForthError) sig m) => Stmt -> m ()
evalStmt = \case
  Num n -> modify @ForthState (second (n :))
  Sym s -> do
    env <- gets @ForthState fst
    case env M.!? s of
      Nothing -> throwError (UnknownWord s)
      Just action -> evalAction action
  WordDecl wName body -> do
    when (T.all isDigit wName) $
      throwError InvalidWord
    e <- gets @ForthState fst
    modify @ForthState (first (M.insert wName $ Closure e body))

initEnv :: M.Map T.Text Action
initEnv =
  M.fromList
    [ declBinOp "+" (\b a st -> pure $ a + b : st)
    , declBinOp "-" (\b a st -> pure $ a - b : st)
    , declBinOp "*" (\b a st -> pure $ a * b : st)
    , declBinOp
        "/"
        (\b a st ->
           if b == 0
             then Left DivisionByZero
             else pure $ a `quot` b : st)
    , declUnaryOp "dup" (\a st -> pure $ a : a : st)
    , declUnaryOp "drop" (\_a st -> pure st)
    , declBinOp "swap" (\b a st -> pure $ a : b : st)
    , declBinOp "over" (\b a st -> pure $ a : b : a : st)
    ]
  where
    declUnaryOp sym uOp =
      ( sym
      , Prim $ \(env, st) -> case st of
          a : st1 -> let st2 = uOp a st1 in (env,) <$> st2
          _ -> Left StackUnderflow
      )
    declBinOp sym binOp =
      ( sym
      , Prim $ \(env, st) -> case st of
          b : a : st1 -> let st2 = binOp b a st1 in (env,) <$> st2
          _ -> Left StackUnderflow
      )

parseForth :: T.Text -> Either String [Stmt]
parseForth = P.parseOnly (P.skipSpace *> many stmt <* P.endOfInput)
  where
    stmt :: P.Parser Stmt
    stmt = wordDecl <|> num <|> symbol
      where
        nonSpaces :: P.Parser T.Text
        nonSpaces = P.takeWhile1 (not . isSpace) <* P.skipSpace
        symbol = do
          x <- nonSpaces
          Sym (T.toLower x) <$ guard (x /= ":" && x /= ";")
        num = Num <$> P.decimal <* P.skipSpace
        wordDecl = do
          ":" >> P.skipSpace
          -- we could just reject the language here if `wName` is nothing but digits,
          -- but it seems the intention is to reject it during evaluation.
          wName <- nonSpaces
          body <- many stmt
          ";" >> P.skipSpace
          pure $ WordDecl (T.toLower wName) body
