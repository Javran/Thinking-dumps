import Utils

main = do
	putExprLn $ 4
	putExprLn $ 4.0
	putExprLn $ 4 + 2.0 * 5
	putExprLn $ 4 * 5 + 1
	putExprLn $ 4 *(5 + 1)
	-- actually both `+` and `*` are functions 
	putExprLn $ (+) ((*) 4 5) 1

	-- String and [Char] are equal
	let a = "Nice"
	let b = ['N', 'i', 'c', 'e']

	putStrLn a
	putStrLn b
	putExprLn $ a == b

	putStrLn $ "Nice" ++ [' ', 'b', 'o', 'a', 't', '!']

	putExprLn $ (4+5) == 9
	-- True
	putExprLn $ (5+5) /= 10
	-- False

	-- line below cannot compile 
	--     since the if statement is incomplete 
	-- putStrLn $ if (5 == 5) then "true"

	putStrLn $ if (5 == 5) then "true" else "false"
