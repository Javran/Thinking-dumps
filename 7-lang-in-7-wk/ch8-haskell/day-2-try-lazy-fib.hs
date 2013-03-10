import Utils

lazyFib x y = x:(lazyFib y (x+y))

-- lazyFib 1 1
-- 1: lazyFib 1 2
-- 1: 1 : lazyFib 2 3
-- ...
fib = lazyFib 1 1

fibNth x = head $ drop (x-1) $ take x fib

main = do
	putExprLn $ map fibNth [1..10]

	putExprLn $ take 10 fib
	putExprLn $ take 10 $ drop 20 $ lazyFib 1 1
	putExprLn $ fibNth 3
	putExprLn $ fibNth 6

	--   1 1 2 3 5 ...
	-- + 1 2 3 5 ...

	putExprLn $ take 10 $ zipWith (+) fib $ drop 1 fib
	putExprLn $ take 5 $ map (*2) [1..]

	putExprLn $ take 5 $ map ( (*2).(*5) ) fib
	-- *5, then *2
