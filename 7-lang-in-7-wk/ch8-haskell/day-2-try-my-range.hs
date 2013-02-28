import Utils

-- an infinite list
myRange start step = start:(myRange (start + step) step)

main = do
	putExprLn $ take 10 $ myRange 10 1
	-- [10,11 .. 19]
