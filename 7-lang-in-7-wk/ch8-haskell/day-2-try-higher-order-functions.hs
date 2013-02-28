import Utils

main = do
	putExprLn $ (\x -> x) "Logical."
	putExprLn $ id "Logical."

	putExprLn $ (\x -> x ++ " captain.") "Logical,"
	putExprLn $ ( ++ " captain." ) "Logical,"

	let testCase = [1..5] :: [Integer]

	-- test case for Fractional
	let testCaseF = map fromIntegral testCase

	putExprLn $ map (\x -> x*x) testCase
	-- [1,4,9 ...]

	let squareAll list = map square list
		where square x = x * x 

	putExprLn $ squareAll testCase
	-- [1,4,9 ...]

	putExprLn $ map (+1) testCase
	-- [2,3,4 ...]

	putExprLn $ map (/2) testCaseF
	-- [0.5, 1.0, 1.5 ...]

	putExprLn $ map (2/) testCaseF
	-- [2.0, 1.0, ...]

	putExprLn $ map odd testCase
	-- [True, False, True ...]

	putExprLn $ foldl (\x carryOver -> carryOver + x) 0 [1..10]
	-- 55
	putExprLn $ foldl (+) 0 [1..10]
	-- 55

	putExprLn $ foldl1 (+) testCase
	-- 15
	putExprLn $ foldl1 (+) testCaseF
	-- 15.0
