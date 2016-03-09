module Day1Easy

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
mkCard : Suit -> Nat -> Maybe Card
mkCard s n = if n >= 0 && n <= 13
  then Just (C s n) 
  else Nothing

-- a deck of cards
cardDeck : List Card
cardDeck = [ C s n | s <- [Clubs, Diamonds, Hearts, Spades], n <- [1..13]] 
