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

backwardsCard :: Hand -> Hand
backwardsCard [] = []
backwardsCard (x:xs) = backwardsCard xs ++ [x]

backwards :: [a] -> [a]
backwards [] = []
backwards (x:xs) = backwards xs ++ [x]

main = do
	let card = (Ten, Hearts)
	putExprLn $ card
	putExprLn $ cardValue card

	let hand = [(Ten, Spades), (Jack, Hearts) ]

	putExprLn $ backwardsCard hand
	-- not work for [Char]
	-- putExprLn $ backwardsCard "abc"

	-- reverse [a], "a" can be anything
	putExprLn $ backwards hand
	putExprLn $ backwards "abc"
