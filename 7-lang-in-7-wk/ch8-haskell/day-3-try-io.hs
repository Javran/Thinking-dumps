import System.IO

tryIo = do
	putStr "Enter your name: "
	-- stdout is line-buffering,
	--     you might need to flush the prompt above manually
	hFlush stdout
	line <- getLine
	let backwards = reverse line
	-- let's eliminate quotes by putStrLn
	putStrLn $ "Hello, Your name backwards is " ++ backwards

main = tryIo
