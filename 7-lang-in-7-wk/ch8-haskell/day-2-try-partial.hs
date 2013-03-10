import Utils

prod1 x y = x * y

prod2 = (*)

main = do
	putExprLn $ prod1 3 4
	putExprLn $ prod2 3 4
	-- 12

	let double = prod1 2
	let triple = (*3)

	putExprLn $ double 10
	-- 20
	putExprLn $ triple 9
	-- 27
