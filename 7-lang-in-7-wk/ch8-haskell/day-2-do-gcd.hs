import Utils

myGCD :: (Integral a) => a -> a -> a
myGCD x 0 = x
myGCD x y = myGCD y $ x `mod` y

main = do
	putStrLn "Task #6: calculate gcd of two integers"
	putExprLn $ myGCD 10 20
	-- 10
	putExprLn $ myGCD 39 1
	-- 1
	putExprLn $ myGCD 432143214321 123412341234
	-- 100010001
	putExprLn $ myGCD 4321 1234
	-- 1
	putExprLn $ myGCD 18446744073709551616 65536
	-- 65536
