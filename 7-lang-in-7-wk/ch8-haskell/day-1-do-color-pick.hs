import Utils

colorList = [
	"black",
	"white",
	"blue",
	"yellow",
	"red"]

allColorPairs = 
	[ (colorA, colorB) |
		colorA <- colorList,
		colorB <- colorList]


colorPairEqual :: (String, String) -> (String, String) -> Bool
colorPairEqual (a,b) (c,d)
	| (a,b) == (c,d) 	= True
	| (a,b) == (d,c) 	= True
	| otherwise 		= False

filterDuplicateColorPairs :: [(String, String)] -> [(String, String)]
filterDuplicateColorPairs = foldl mayCollect [] where
	mayCollect set item = if filter (\x -> colorPairEqual x item) set == []
				 	-- if the item has not yet been included
					then set ++ return item
					else set

-- Task #3: pick a pair of colors from 
--     black, white, blue, yellow and red
--     note: can include either (black, blue) or (blue, black) in the result

main = do
	putStrLn "All possible combinations:"
	mapM_ putExprLn $ filterDuplicateColorPairs allColorPairs
