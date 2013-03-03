import Utils

-- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
-- FYI: haskell.org also provides some articles on how to produce a list of primes:
--     http://www.haskell.org/haskellwiki/Prime_numbers

-- filter out all elements e that e `mod` x == 0
filterOut :: (Integral a) => a -> [a] -> [a]
filterOut x list = filter (\e -> e `mod` x /= 0) list

generatePrimeList list = primeNum : (filterOut primeNum list) where
	primeNum = head list

primeList = generatePrimeList [2..]


main = do
	putStrLn "Task #7: prime list"
	putExprLn $ take 40 $ generatePrimeList [2..]
