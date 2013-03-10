import Utils

allEven :: [Integer] -> [Integer]
allEven [] = []
allEven (x:xs) =
	if even x 
		then x:restEven
		else restEven 
			where restEven = allEven xs

main = do
	let h:t = [1, 2, 3]

	putExprLn h
	-- 1
	putExprLn t
	-- [2, 3]

	putExprLn $ h:t
	putExprLn $ 1:2:3:[]
	putExprLn $ [1]:[[2],[3,4]]
	putExprLn $ [1]:[]

	putExprLn $ [1..2]
	putExprLn $ [1..4]
	putExprLn $ [10..4]
	-- []
	putExprLn $ [10,9..4]
	putExprLn $ [10,8..4]
	putExprLn $ [10,9.5..4]

	-- lazy evaluation:

	putExprLn $ take 5 [1..]
	-- just like clojure, we can use drop and take
	putExprLn $ drop 2 $ take 10 [1..]
	-- [3 .. 10]

	putExprLn $ take 5 [0,2..]
