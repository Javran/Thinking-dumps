import Utils

-- make a multiplication table: (x, y, x*y)
--     where x = start, y <- [start .. end]
mkMultiplicationTable start end =
	[ (x, y, x*y) | x <- return start, y <-[start..end]]

multiplicationTable = map (\row -> mkMultiplicationTable row 12) [1..12]

stringJoin :: String -> [String] -> String
stringJoin separator xs = foldl (\acc i -> acc ++ separator ++ i) (head xs) (tail xs)

main = do
	mapM_ putTableLineLn  multiplicationTable where
		putTableLineLn tableLine = putStrLn $ stringJoin " " $ map tupleToStr tableLine
		tupleToStr (x,y,z) = show x ++ "x" ++ show y ++ "=" ++ show z
