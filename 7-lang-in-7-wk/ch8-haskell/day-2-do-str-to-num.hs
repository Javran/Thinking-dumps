import Control.Monad

import Utils

isNum :: Char -> Bool
isNum = (`elem` ['0'..'9'])

-- check and remove the leading '$' 
strCheckHead :: String -> Maybe String
strCheckHead rawStr = 
	if head rawStr /= '$'
	     	-- should have a leading '$'
		then Nothing
		else Just $ tail rawStr

-- separateStr into integer part and decimal part
--     e.g. "123,456.78" -> ( "123,456", 78 ) 
strSeparateNum :: String -> Maybe (String, Int)
strSeparateNum rawStr = 
	-- should have 2 decimal digits
	if (head $ drop 2 $ reverse rawStr) /= '.'
		then Nothing
		else
			if not $ all isNum  $ take 2 $ reverse rawStr
				then Nothing
				else Just (intPartStr, decPart) where
					-- drop the last 3 Char to get the body
					intPartStr = reverse $ drop 3 $ reverse rawStr
					decPart = read $ reverse $ take 2 $ reverse rawStr

-- check if the given string has correctly placed commas
--     and try to convert the String into Integer
strCheckCommas :: String -> Maybe Integer
strCheckCommas rawStr = 
	if not $ all 
   	-- check the validity of each Char according to their indices
		(\ (ind, ch) -> if (ind `mod` 4 == 0)
			then ch == ','
			else isNum ch
		)
    		-- bind raw string with index
			(zip [1..] $ reverse rawStr)
	then Nothing
	else Just $ read $ filter (/= ',') rawStr

strToNum :: String -> Maybe Double
strToNum rawStr = do
	tmp1 <- strCheckHead rawStr
	(intPartStr, decPart) <- strSeparateNum tmp1
	intPart <- strCheckCommas intPartStr
	return $ (fromIntegral intPart) + (fromIntegral decPart) / 100

main = do
	let testCases =
		[ ( "$not a num", 		Nothing )
		, ( "$123,456.789", 		Nothing )
		, ( "123,456,789.00", 		Nothing )
		, ( "$12,34,56,78.00", 		Nothing )
		, ( "$1234,567,890.11", 	Nothing )
		, ( "$123.000", 		Nothing )
		, ( "$123.00", 			Just 123)
		, ( "$2,345,678.99", 		Just 2345678.99)
		, ( "$002,345,678.99", 		Just 2345678.99)
		, ( "$00,000,000,000.00", 	Just 0)
		, ( "$0,000,000,001.23", 	Just 1.23)]

	putStrLn "Task #3: convert strings to numbers"

	putStrLn "Let's do some tests!"

	-- collect test results
	let testResults = mapM doTest testCases where
		doTest (raw, expected) = do
			putStrLn "Input:"
			putStrLn raw
			putStrLn "Expected result:"
			putExprLn expected
			if strToNum raw == expected
				then do
					putStrLn "Test passed"
					return True
				else do
					putStrLn "Test failed"
					return False

	-- check if all tests are passed
	passed <- liftM and testResults	 
	if passed
		then putStrLn "All tests passed."
		else putStrLn "Some tests failed."
