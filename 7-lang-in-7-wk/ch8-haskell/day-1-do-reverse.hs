import Utils

reverseNative = reverse

reverseRecur :: [a] -> [a]
reverseRecur [] = []
reverseRecur (x:xs) = reverseRecur xs ++ return x

reverseFoldl :: [a] -> [a]
reverseFoldl = foldl reverseCollect [] where
	reverseCollect coll x = x:coll

-- Task #2: reverse a list
main = do
	let testCase = [1..10]
	putExprLn $ reverseNative testCase
	putExprLn $ reverseRecur testCase
	putExprLn $ reverseFoldl testCase
