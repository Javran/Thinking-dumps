import Utils

main = do
	let cartesian (xs,ys) = do
		x <- xs
		y <- ys
		return (x,y)
	
	putExprLn $ curry cartesian [1..2] [3..4]

	-- actually, list comprehension is implemented by monad
	-- let's see:
	let cartesianM (xs, ys) = 
		[ (x,y) |
			  x <- xs
			, y <- ys] 

	putExprLn $ curry cartesianM [1..2] [3..4]
