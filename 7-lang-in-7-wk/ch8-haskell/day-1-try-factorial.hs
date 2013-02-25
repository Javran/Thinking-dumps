import Utils

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

factorialWithGuard :: Integer -> Integer
factorialWithGuard x
	| x > 1 	= x * factorialWithGuard ( x - 1 )
	| otherwise 	= 1

main = do
	putStrLn "Factorial list [1..10]:"
	mapM_ (putExprLn.factorial) [1..10]

	putStrLn "Repeat with factorialWithGuard:"
	mapM_ (putExprLn.factorialWithGuard) [1..10]
