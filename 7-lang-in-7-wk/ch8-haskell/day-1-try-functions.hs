import Utils
import Data.Dynamic

-- please refer to:
--     http://stackoverflow.com/questions/15052793/convert-expression-into-the-string-describing-its-type-in-haskell-script
-- if you have interest in how to get the type of an expression and convert it into String

main = do
	let x = 10
	putExprLn x
	--10

	let double x = x * 2
	putExprLn $ double 2
	-- 4

	putExprLn $ double 5
	-- 10

	let doubleInt x = x * 2 :: Integer

	putStrLn "The type of doubleInt is: "
	putStrLn $ show $ typeOf doubleInt

	let fact x =
		if x == 0 then
			1
		else
			fact (x - 1) * x

	putExprLn $ fact 3
	-- 6
