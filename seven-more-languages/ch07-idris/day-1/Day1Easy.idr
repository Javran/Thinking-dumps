module Day1Easy

-- see day 2 of the book
import Data.So

-- find numbers greater than a given number
filterGreaterThan : Ord a => a -> List a -> List a
filterGreaterThan v xs = filter (\x => x > v) xs

-- find every other member of a list
everyOther : List a -> List a
everyOther (a::_::xs) = a :: everyOther xs
everyOther (a::[]) = [a]
  -- the last pattern could have been a wildcard pattern,
  -- but here we give every pattern explicitly to show that
  -- Idris knows this is a total function.
everyOther [] = []

-- build a data type representing a playing card from a standard poker deck
data Suit
  = Clubs | Diamonds | Hearts | Spades

data Card = C Suit Nat

-- safe constructor
mkCard : Suit -> (n : Nat) -> Maybe Card
mkCard s n = if n >= 1 && n <= 13
  then Just (C s n) 
  else Nothing


-- see day2 of the book
validCardNumRange : Nat -> Bool
validCardNumRange n = n >= 1 && n <= 13

data Card2 : Suit -> Nat -> Type where
  SafeCard2 : (s : Suit) -> (n : Nat)  -> So (validCardNumRange n) -> Card2 s n

-- safe constructor version 2: using dependent type
mkCard2 : (s : Suit) -> (n : Nat) -> Maybe (Card2 s n)
mkCard2 s n = case choose (validCardNumRange n) of
  Left valid => Just (SafeCard2 s n valid)
  Right _ => Nothing

-- a deck of cards
cardDeck : List Card
cardDeck = [ C s n | s <- [Clubs, Diamonds, Hearts, Spades], n <- [1..13]] 
