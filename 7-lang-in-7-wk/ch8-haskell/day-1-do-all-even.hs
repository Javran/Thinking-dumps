import Control.Monad
import Utils

testAllEven :: ([Integer] -> [Integer]) -> Bool 
testAllEven f = (f testCase) == expectedCaseResult where
	testCase = [1..10]
	expectedCaseResult = [2,4..10]

testPrints :: ([Integer] -> [Integer]) -> IO()
testPrints f = do
	let result = testAllEven f
	if result
		then
			putStrLn "Test passed"
		else
			putStrLn "Test failed"
			
allEvenWithFilter = filter even

allEvenWithFoldl = foldl collectEven [] where
	collectEven coll x = if even x
				then coll ++ [x]
				else coll

allEvenWithRecursion [] = []
allEvenWithRecursion (x:xs) = if even x
					then x:rest
					else rest
						where rest = allEvenWithRecursion xs

allEvenWithListComprehension [] = []
allEvenWithListComprehension xs = [x | x <- xs, even x]

allEvenWithListMonad [] = []
allEvenWithListMonad xs = do
	x <- xs
	guard $ even x
	return x

-- actually allEvenWithListComprehension and allEvenWithListMonad is identical
-- refer to: http://learnyouahaskell.com/a-fistful-of-monads#the-list-monad

main = mapM_ testPrints [
	allEvenWithFilter,
	allEvenWithFoldl,
	allEvenWithRecursion,
	allEvenWithListComprehension,
	allEvenWithListMonad
	]
