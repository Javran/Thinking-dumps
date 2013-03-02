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
   	-- check the validity of each Char according to their index
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
	putExprLn $ return "$2,345,678.99" >>= strCheckHead >>= strSeparateNum
	putExprLn $ strToNum "$2,345,678.99"
