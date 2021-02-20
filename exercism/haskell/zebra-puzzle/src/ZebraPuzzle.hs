{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}

module ZebraPuzzle
  ( Resident (..)
  , Solution (..)
  , solve
  )
where

import Control.Applicative
import Control.Monad
import Data.List

data Resident
  = Englishman
  | Spaniard
  | Ukrainian
  | Norwegian
  | Japanese
  deriving (Eq, Show, Enum, Bounded)

data HouseColor
  = Red
  | Green
  | Ivory
  | Yellow
  | Blue
  deriving (Eq, Enum, Bounded)

data Pet
  = Dog
  | Snails
  | Fox
  | Horse
  | Zebra
  deriving (Eq, Enum, Bounded)

data Beverage
  = Coffee
  | Tea
  | Milk
  | OrangeJuice
  | Water
  deriving (Eq, Enum, Bounded)

data Cigarette
  = OldGold
  | Kools
  | Chesterfields
  | LuckyStrike
  | Parliaments
  deriving (Eq, Enum, Bounded)

data Solution = Solution
  { waterDrinker :: Resident
  , zebraOwner :: Resident
  }
  deriving (Eq, Show)

solve :: Solution
solve = head solutions

allPerms :: (Enum a, Bounded a) => [] [a]
allPerms = permutations [minBound .. maxBound]

{-
  While the order of those statements can certainly be rearranged
  to eliminate some invalid states sooner rather than later,
  but the search space is so small it won't matter much.

  Here I choose to arrange them in the order they are described in the exercise,
  to make mistakes more obvious (should any).
 -}
solutions :: [] Solution
solutions = do
  residents <- allPerms @Resident
  houses <- allPerms @HouseColor
  -- The Englishman lives in the red house.
  do
    let Just i = elemIndex Englishman residents
    guard $ houses !! i == Red

  pets <- allPerms @Pet
  -- The Spaniard owns the dog.
  do
    let Just i = elemIndex Spaniard residents
    guard $ pets !! i == Dog

  beverages <- allPerms @Beverage
  -- Coffee is drunk in the green house.
  do
    let Just i = elemIndex Coffee beverages
    guard $ houses !! i == Green
  -- The Ukrainian drinks tea.
  do
    let Just i = elemIndex Ukrainian residents
    guard $ beverages !! i == Tea
  -- The green house is immediately to the right of the ivory house.
  do
    let Just i = elemIndex Ivory houses
    guard $ i + 1 < 5 && houses !! (i + 1) == Green
  -- The Old Gold smoker owns snails.
  cigarettes <- allPerms @Cigarette
  do
    let Just i = elemIndex OldGold cigarettes
    guard $ pets !! i == Snails

  -- Kools are smoked in the yellow house.
  do
    let Just i = elemIndex Kools cigarettes
    guard $ houses !! i == Yellow
  -- Milk is drunk in the middle house.
  guard $ beverages !! 2 == Milk

  -- The Norwegian lives in the first house.
  guard $ head residents == Norwegian

  -- The man who smokes Chesterfields lives in the house next to the man with the fox.
  do
    let Just i = elemIndex Chesterfields cigarettes
    guard (i - 1 >= 0 && pets !! (i -1) == Fox)
      <|> guard (i + 1 < 5 && pets !! (i + 1) == Fox)
  -- Kools are smoked in the house next to the house where the horse is kept.
  do
    let Just i = elemIndex Kools cigarettes
    guard (i - 1 >= 0 && pets !! (i -1) == Horse)
      <|> guard (i + 1 < 5 && pets !! (i + 1) == Horse)
  -- The Lucky Strike smoker drinks orange juice.
  do
    let Just i = elemIndex LuckyStrike cigarettes
    guard $ beverages !! i == OrangeJuice

  -- The Japanese smokes Parliaments.
  do
    let Just i = elemIndex Japanese residents
    guard $ cigarettes !! i == Parliaments
  -- The Norwegian lives next to the blue house.
  do
    let Just i = elemIndex Norwegian residents
    guard (i - 1 >= 0 && houses !! (i -1) == Blue)
      <|> guard (i + 1 < 5 && houses !! (i + 1) == Blue)

  waterDrinker <- do
    let Just i = elemIndex Water beverages
    pure $ residents !! i
  zebraOwner <- do
    let Just i = elemIndex Zebra pets
    pure $ residents !! i
  pure $ Solution {waterDrinker, zebraOwner}
