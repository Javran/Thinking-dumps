{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo #-}

module DND
  ( Character (..)
  , ability
  , modifier
  , character
  )
where

import Control.Monad
import Data.Semigroup
import Test.QuickCheck

data Character = Character
  { strength :: Int
  , dexterity :: Int
  , constitution :: Int
  , intelligence :: Int
  , wisdom :: Int
  , charisma :: Int
  , hitpoints :: Int
  }
  deriving (Show, Eq)

modifier :: Int -> Int
modifier = (`div` 2) . subtract 10

ability :: Gen Int
ability = do
  xs <- replicateM 4 (choose (1, 6))
  pure $
    let (Sum a, Min b) =
          foldMap ((,) <$> Sum <*> Min) xs
     in a - b

character :: Gen Character
character = mdo
  ch@Character {constitution} <-
    Character
      <$> ability
      <*> ability
      <*> ability
      <*> ability
      <*> ability
      <*> ability
      <*> pure (10 + modifier constitution)
  pure ch
