{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Bowling
  ( score
  , BowlingError (..)
  )
where

import Control.Monad.Except
import Control.Monad.RWS.Strict

data BowlingError
  = IncompleteGame
  | InvalidRoll {rollIndex :: Int, rollValue :: Int}
  | InvalidGameState String
  deriving (Eq, Show)

type M = RWST () (Sum Int) Game (Except BowlingError)

data Game = Game
  { gFrame :: Int -- game starts at Frame 1
  , gIndex :: Int -- keeps track of rollIndex for error reporting
  , gScores :: [Int] -- current front for current frame
  }

score :: [Int] -> Either BowlingError Int
score rolls =
  runExcept $ do
    ((), _, Sum i) <-
      runRWST
        (replicateM_ 10 scoreNextFrame)
        ()
        (Game
           { gFrame = 1
           , gIndex = 0
           , gScores = rolls
           })
    pure i

scoreNextFrame :: M ()
scoreNextFrame = do
  frame <- gets gFrame
  unless (frame <= 10) $
    throwError $ InvalidGameState "no more than 10 frames"
  let recordScore = tell . Sum
      nextRoll = do
        Game {gScores = scores, gIndex = rInd} <- get
        case scores of
          [] -> throwError IncompleteGame
          x : _ ->
            if x < 0 || x > 10
              then throwError $ InvalidRoll rInd x
              else do
                modify
                  (\g@Game {gScores = _ : gScores', gIndex} ->
                     g {gScores = gScores', gIndex = gIndex + 1})
                pure x
  x <- nextRoll
  case x of
    10 -> do
      -- a strike
      bonus <- gets (sum . take 2 . gScores)
      when (frame == 10) $ do
        a <- nextRoll
        b <- nextRoll
        when (a /= 10 && a + b > 10) $ do
          rInd <- gets gIndex
          {-
            somewhat arbitrarily, test suite decides that,
            if a and b looks fine but a+b exceeds 10, it's a's fault.
           -}
          throwError $ InvalidRoll (rInd -1) b
      recordScore $ 10 + bonus
    _ -> do
      y <- nextRoll
      when (x + y > 10) $ do
        rInd <- gets gIndex
        throwError $ InvalidRoll (rInd -1) y
      if x + y == 10
        then do
          -- a spare
          bonus <- gets (sum . take 1 . gScores)
          recordScore $ 10 + bonus
          when (frame == 10) $
            -- consume for validation.
            void nextRoll
        else do
          recordScore $ x + y
  when (frame == 10) $ do
    sc <- gets gScores
    unless (null sc) $ do
      rInd <- gets gIndex
      throwError $ InvalidRoll rInd (head sc)
  modify (\g@Game {gFrame} -> g {gFrame = gFrame + 1})
