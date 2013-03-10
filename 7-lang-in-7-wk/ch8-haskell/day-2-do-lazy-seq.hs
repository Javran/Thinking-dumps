import Utils

arithmeticSeq initV increment = initV : arithmeticSeq (initV+increment) increment

main = do
	let lazySeq1 = flip arithmeticSeq 2
	let lazySeq2 = flip arithmeticSeq 4

	putStrLn "Task #4: lazy seq and combination"

	putStrLn "Test two lazy seq:"
	putExprLn $ take 10 $ lazySeq1 2
	putExprLn $ take 10 $ lazySeq2 2

	-- although the task asks us to combine two functions together
	--     I'm wondering how can two functions
	--     that have type: (Num a) -> a -> [a] combined together
	--     given that (.) :: (b -> c) -> (a -> b) -> a -> c
	-- I'll use the two functions in the definition of `lazySeq`,
	--     but conspicuously it is not a function combination
	let lazySeq3 x y = zipWith (+) 
				(zipWith (+) (lazySeq1 x) (lazySeq2 y))
				[0..]

	putStrLn "Test the final lazy seq:"
	putExprLn $ take 10 $ lazySeq3 1 1
