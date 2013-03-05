import Utils

data Suit = Spades | Hearts
	deriving (Show)

data Rank = Ten | Jack | Queen | King | Ace
	deriving (Show)

type Card = (Rank, Suit)

type Hand = [Card]

value :: Rank -> Integer
value Ten 	= 1
value Jack 	= 2
value Queen 	= 3
value King 	= 4
value Ace 	= 5

cardValue :: Card -> Integer
cardValue (rank, _) = value rank

main = do
	let card = (Ten, Hearts)
	putExprLn $ card
	putExprLn $ cardValue card
