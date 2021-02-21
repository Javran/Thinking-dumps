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
  { -- | Game starts at Frame 1
    gFrame :: Int
  , -- | keeps track of rollIndex for error reporting
    gIndex :: Int
  , -- | current front roll scores aligned with current frame
    gScores :: [Int]
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
  when (frame > 10) $
    throwError $ InvalidGameState "no more than 10 frames"
  let recordScore = tell . Sum
      invalidRoll v = do
        rInd <- gets gIndex
        throwError $ InvalidRoll rInd v
      invalidRoll' v = do
        {-
          somewhat arbitrarily, test suite decides that,
          if a and b looks fine but a+b exceeds 10,
          the blame is on a's index and b's rollValue,
          which makes as much sense as having two `invalidRoll` functions.
         -}
        rInd <- gets gIndex
        throwError $ InvalidRoll (rInd -1) v
      -- consumes and verifies next roll.
      nextRoll = do
        Game {gScores = scores} <- get
        case scores of
          [] -> throwError IncompleteGame
          x : _ ->
            if x < 0 || x > 10
              then invalidRoll x
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
        when (a /= 10 && a + b > 10) $
          invalidRoll' b
      recordScore $ 10 + bonus
    _ -> do
      y <- nextRoll
      when (x + y > 10) $
        invalidRoll' y
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
      invalidRoll (head sc)
  modify (\g@Game {gFrame} -> g {gFrame = gFrame + 1})
