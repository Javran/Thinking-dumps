import Utils

data Color = 
	  Red
	| Green
	| Blue 
	deriving (Show, Eq, Enum, Bounded)

allColor :: [Color]
allColor = [minBound .. maxBound]

data Location =
	  Alabama
	| Mississippi
	| Georgia
	| Tennessee
	| Florida
	deriving (Show, Eq, Enum, Bounded)

connectionList = [
	  (Mississippi, Tennessee)
	, (Mississippi, Alabama)
	, (Alabama, Tennessee)
	, (Alabama, Georgia)
	, (Alabama, Florida)
	, (Georgia, Florida)
	, (Georgia, Tennessee)]

main = do
	let searchSpace = 
		[[ 	  (Alabama, c1)
			, (Mississippi, c2)
			, (Georgia, c3)
			, (Tennessee, c4)
			, (Florida, c5) ] | 
			  c1 <- allColor
			, c2 <- allColor
			, c3 <- allColor
			, c4 <- allColor
			, c5 <- allColor]
	putStrLn "All possible solutions:"
	-- pick all vaild answer from search space, and print them
	mapM_ putExprLn $ filter validAnswer searchSpace where
		-- we will verify each possible answer by taking connectionList into account
		-- the correct answer must meet all connection constraints given
		validAnswer answer = all checkCondition connectionList where
			-- check condition: when x,y connects, the color of them must be different
			checkCondition (x,y) = colorOf x /= colorOf y where
				colorOf loc = snd $ head $ filter (\(l, _) -> loc == l) answer
