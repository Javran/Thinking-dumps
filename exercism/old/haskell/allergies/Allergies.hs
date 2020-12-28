module Allergies
  ( Allergen(..)
  , isAllergicTo
  , allergies
  ) where

import Data.Bits

data Allergen
  = Eggs
  | Peanuts
  | Shellfish
  | Strawberries
  | Tomatoes
  | Chocolate
  | Pollen
  | Cats
    deriving (Enum, Bounded, Eq, Show)

toBit :: Allergen -> Int
toBit = fromEnum

isAllergicTo :: Allergen -> Int -> Bool
isAllergicTo = gIsAllergicTo

allergies :: Int -> [Allergen]
allergies = gAllergies

universe :: (Enum a, Bounded a) => [a]
universe = [minBound .. maxBound]

gIsAllergicTo :: Bits a => Allergen -> a -> Bool
gIsAllergicTo a n = a `elem` gAllergies n

gAllergies :: Bits a => a -> [Allergen]
gAllergies n = concatMap (\a -> [a | testBit n (toBit a)]) universe
