import Data.Ratio

import Utils

main = do
	putStrLn "Task #5: partial functions"

	-- a function that returns half of the given number
	let 
		half :: (Fractional a) => a -> a
		half = (/2)

	-- a function that appends '\n' to all Strings given
	let appendNewline = (++"\n")

	putStrLn "Test half:"
	putExprLn $ half 4
	putExprLn $ half 3
	putExprLn $ half (1 % 2)

	putStrLn "Test appendNewline:"
	putExprLn $ appendNewline "Nice"
	putExprLn $ appendNewline "Boat"
