module Poker
  ( bestHands
  )
where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP

type Card = (Int, Char) -- (<rank>, <suit>)

type Hand = [Card]

parseHand :: String -> Maybe Hand
parseHand raw = case readP_to_S (hand <* eof) raw of
  [(v, "")] -> Just v
  _ -> Nothing
  where
    suit = satisfy (`elem` "DHCS")
    rank =
      (10 <$ string "10")
        <++ (14 <$ char 'A')
        <++ (13 <$ char 'K')
        <++ (12 <$ char 'Q')
        <++ (11 <$ char 'J')
        <++ (satisfy isDigit >>= \x -> pure $ ord x - ord '0')

    card :: ReadP Card
    card = (,) <$> rank <*> suit

    hand = do
      xs@[_, _, _, _, _] <- card `sepBy1` char ' '
      pure xs

{-
  For ranking hands.

  - Alternatives are arranged from lowest to highest rank,
    this ensures that higher rank always beats lower rank regardless of numeric value.

  - Suit are dropped from this representation as it's exact value is irrelevant to
    this ranking method.

  - Within the same alternative construct, each value represents a card rank,
    ordered from largest count to lowest count.
    If card count is the same, larger numeric value goes first.

 - whenever [Int] appears, it's always of size 5

  For example:
  - FourOfAKind 3 2 represents 3,3,3,3,2,2
  - TwoPair 14 13 12 represents A,A,K,K,Q
  - StraightFlush 14 represents A,K,Q,J,10 (a.k.a. Royal flush)

 -}
data HandRank
  = HighCard [Int]
  | OnePair Int Int Int Int
  | TwoPair Int Int Int
  | ThreeOfAKind Int Int Int
  | Straight Int
  | Flush [Int]
  | FullHouse Int Int
  | FourOfAKind Int Int
  | StraightFlush Int
  deriving (Eq, Ord)

toHandRank :: Hand -> HandRank
toHandRank hand =
  if suitCount == 1
    then -- This is at least a Flush
    case mayStraight of
      Just v -> StraightFlush v
      Nothing -> Flush (fmap fst rankFreqDesc)
    else case mayStraight of
      Just v -> Straight v
      Nothing ->
        {-
          Due to the fact that a hand has exactly 5 cards, the following
          patterns are actually exhaustive:
          - [_,_]: 2 distinct values, either Four of a Kind or Full House
          - [_,_,_]: 3 distinct values, either Three of a Kind or Two Pair
          - [_,_,_,_]: 4 distinct values, must be One Pair
          - otherwise: 5 distinct values, must be High Card
         -}
        case rankFreqDesc of
          [(a, 4), (b, _)] -> FourOfAKind a b
          [(a, 3), (b, _)] -> FullHouse a b
          [(a, 3), (b, _), (c, _)] -> ThreeOfAKind a b c
          [(a, 2), (b, _), (c, _)] -> TwoPair a b c
          [(a, 2), (b, _), (c, _), (d, _)] -> OnePair a b c d
          _ -> HighCard $ fmap fst rankFreqDesc
  where
    suitCount = length $ nub $ fmap snd hand

    -- a list of <rank, # of cards>, descrending by count then rank.
    rankFreqDesc =
      sortBy (\(r0, c0) (r1, c1) -> compare c1 c0 <> compare r1 r0) rankFreq
      where
        rankFreq =
          fmap (\l@(x : _) -> (x, length l)) . group $ sort $ fmap fst hand

    {-
      Note that we don't really need any special handling for Ace,
      with the exception of mayStraight.

      The observation is, the only case in which Ace is considered 1 is when
      it is required to complete a straight.
     -}
    mayStraight :: Maybe Int
    mayStraight = do
      -- all card ranks must be unique for this to be possible
      guard $ all ((== 1) . snd) rankFreqDesc
      let ranksDesc = fmap fst rankFreqDesc
          hd = head ranksDesc
      (-- descrending sequence of 5 elements
       hd <$ guard (and $ zipWith (==) ranksDesc [hd, hd -1 ..]))
        <|> (-- or Ace as 1
             5 <$ guard (ranksDesc == [14, 5, 4, 3, 2]))

bestHands :: [String] -> Maybe [String]
bestHands rawHands = do
  let parseAndRank raw = do
        hand <- parseHand raw
        pure (raw, Down $ toHandRank hand)
  hands <- mapM parseAndRank rawHands
  let ranked = sortBy (comparing snd) hands
  case ranked of
    [] -> Nothing
    ((raw, rank) : xs) -> Just $ raw : (fst <$> takeWhile ((== rank) . snd) xs)
