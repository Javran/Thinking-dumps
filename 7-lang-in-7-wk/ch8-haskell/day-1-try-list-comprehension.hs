import Utils

main = do
	putExprLn [ x * 2 | x <- [1, 2, 3]]
	-- 2 4 6

	let testPoints = [(1,2), (2,3), (3,1)]
	putExprLn [ (y,x) | (x,y) <- testPoints ]
	putExprLn [ (4-x, y) | (x,y) <- testPoints ]

	let crew = ["Kirk", "Spock", "McCoy"]
	putExprLn [ (a,b) | a <- crew, b <- crew ]
	putExprLn [ (a,b) | a <- crew, b <- crew, a < b ]
