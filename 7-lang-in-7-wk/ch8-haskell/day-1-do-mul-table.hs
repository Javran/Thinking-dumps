import Utils

-- make a multiplication table: (x, y, x*y)
--     where x = start, y <- [start .. end]
-- e.g. when (start, end) = (2, 4), result = [(2,2,4),(2,3,6],(2,4,8)]
mkMultiplicationTable start end =
	[ (x, y, x*y) | x <- return start, y <-[start..end]]

-- the table will be:
-- [
--     [ (1,1,1), (1,2,2) ..., (1,12,12) ]
--     [ (2,2,4), (2,3,6) ..., (2,12,24) ]
--     ...
--     [ (12,12,144) ]
-- ]
multiplicationTable = map (\row -> mkMultiplicationTable row 12) [1..12]

stringJoin :: String -> [String] -> String
stringJoin separator xs = foldl (\acc i -> acc ++ separator ++ i) (head xs) (tail xs)

main = do
	mapM_ putTableLineLn  multiplicationTable where
		-- format and print each line, e.g. [ (2,2,4), (2,3,6) ..., (2,12,24) ]
		--     will turns into "2x2=4 2x3=6 ... 2x12=24"
		putTableLineLn tableLine = putStrLn $ stringJoin " " $ map tupleToStr tableLine
		tupleToStr (x,y,z) = show x ++ "x" ++ show y ++ "=" ++ show z
