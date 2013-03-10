import Utils

size :: [a] -> Integer
size [] = 0
size (h:t) = 1 + size t

prod :: Num a => [a] -> a
prod [] = 1
prod (h:t) = h * prod t

main = do
	let (h:t) = [1..4]

	putExprLn h
	putExprLn t

	putExprLn $ size "Fascinating."
	-- 12

	putExprLn $ prod [1 .. 10]
	-- 10 !
	putExprLn $ foldl (*) 1 [1..10]
	-- 10 !
	putExprLn $ product [1..10]
	-- 10 !

	putExprLn $ zip "kirk" "spock"
	putExprLn $ (,) "kirk" "spock"

	putExprLn $ zipWith (\a b -> [a,b]) "kirk" "spock"

	putExprLn $ zip ["kirk", "spock"] ["enterprise", "reliant"]
