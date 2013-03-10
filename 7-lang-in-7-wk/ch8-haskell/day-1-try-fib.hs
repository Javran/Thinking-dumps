import Utils

fib :: Integer -> Integer
fib 0 = 0 -- yes, here should be 0 actually
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

testFib :: (Integer -> Integer) -> [Integer] -> IO ()
testFib fib testCase = do
	putStrLn "Test Fibinacci sequence:"
	mapM_ (putExprLn . fib) testCase

fibTuple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)
fibTuple (x, y, 0) = (x, y, 0)
fibTuple (x, y, index) = fibTuple (y, x+y, index - 1)

fibQuicker :: Integer -> Integer
fibQuicker x = fibResult $ fibTuple (0, 1, x)
	where
		fibResult (x, _, _) = x

fibNextPair :: (Integer, Integer) -> (Integer, Integer)
fibNextPair (x, y) = (y, x+y)

fibNthPair :: Integer -> (Integer, Integer)
fibNthPair 1 = (1, 1)
fibNthPair n = fibNextPair $ fibNthPair $ n - 1

fibWithPair :: Integer -> Integer
fibWithPair = fst . fibNthPair

main = do
	testFib fib [1..10]

	putExprLn $ fibTuple(0,1,4)
	-- (3, 5, 0)

	testFib fibQuicker [1..10]

	putStrLn "Fib 100:"
	putExprLn $ fibQuicker 100

	putStrLn "Fib 1000:"
	putExprLn $ fibQuicker 1000

	testFib fibWithPair [1..10]
