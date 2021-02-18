{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Control.Effect.Error
import Control.Effect.State
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
  deriving (Show, Eq)

type ForthState = (Env, [Int])

data Stmt
  = Num Int
  | Sym T.Text
  | WordDecl T.Text [Stmt]
  deriving (Show)

type Env = M.Map T.Text Action

data Action
  = Prim (ForthState -> Either ForthError ForthState)
  | Closure Env [Stmt]

type EvalCtx = (Env, ForthState)

emptyState :: ForthState
emptyState = (initEnv, [])

evalText :: T.Text -> ForthState -> Either ForthError ForthState
evalText text st = case run $ runError $ runState st $ mapM evalStmt parsed of
  Left err -> Left err
  Right (st, _) -> Right st
  where
    Right parsed = parseForth text

toList :: ForthState -> [Int]
toList = reverse . snd

evalAction :: (Has (State ForthState) sig m, Has (Error ForthError) sig m) => Action -> m ()
evalAction = \case
  Prim prim -> do
    st <- get
    case prim st of
      Left err -> throwError err
      Right st' -> put st'
  Closure cEnv body -> do
    (st :: ForthState) <- get
    let (fEnv, _) = st
    modify (first (const cEnv) :: ForthState -> ForthState)
    mapM evalStmt body
    modify (first (const fEnv) :: ForthState -> ForthState)

evalStmt :: (Has (State ForthState) sig m, Has (Error ForthError) sig m) => Stmt -> m ()
evalStmt = \case
  Num n -> modify (second (n :) :: ForthState -> ForthState)
  Sym s -> do
    (env :: Env) <- gets (fst :: ForthState -> Env)
    case env M.!? s of
      Nothing -> throwError (UnknownWord s)
      Just action -> evalAction action
  WordDecl wName body -> do
    when (T.all isDigit wName) $
      throwError InvalidWord
    e <- gets (fst :: ForthState -> Env)
    let clo = Closure e body
    modify (first (M.insert wName clo) :: ForthState -> ForthState)

initEnv :: M.Map T.Text Action
initEnv =
  M.fromList
    [ declBinOp "+" (\b a st -> a + b : st)
    , declBinOp "-" (\b a st -> a - b : st)
    , declBinOp "*" (\b a st -> a * b : st)
    , ( "/"
      , Prim $ \(env, st) -> case st of
          b : a : st1 ->
            if b == 0
              then Left DivisionByZero
              else let st2 = (a `quot` b) : st1 in Right (env, st2)
          _ -> Left StackUnderflow
      )
    , declUnaryOp "dup" (\a st -> a : a : st)
    , declUnaryOp "drop" (\_a st -> st)
    , declBinOp "swap" (\b a st -> a : b : st)
    , declBinOp "over" (\b a st -> a : b : a : st)
    ]
  where
    declUnaryOp sym uOp =
      ( sym
      , Prim $ \(env, st) -> case st of
          a : st1 -> let st2 = uOp a st1 in Right (env, st2)
          _ -> Left StackUnderflow
      )
    declBinOp sym binOp =
      ( sym
      , Prim $ \(env, st) -> case st of
          b : a : st1 -> let st2 = binOp b a st1 in Right (env, st2)
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
