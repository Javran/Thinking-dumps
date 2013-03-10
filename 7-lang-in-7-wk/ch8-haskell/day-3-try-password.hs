import Control.Monad

import Utils

crack = do
	let charList = "abc"

	x <- charList
	y <- charList
	z <- charList
	let password = [x,y,z]

	return (password, attempt password) where
		attempt pw = pw == "cab"
	
crackPlus = do
	result <- crack
	guard $ snd result
	return $ fst result

main = do
	putExprLn $ crack

	putStrLn "Filterd result:"
	putExprLn $ crackPlus
