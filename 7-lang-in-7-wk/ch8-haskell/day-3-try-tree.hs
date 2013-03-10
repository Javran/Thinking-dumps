import Utils

data Tree a = Children [Tree a] | Leaf a
	deriving (Show)

depth (Leaf _) = 1
depth (Children c) = 1 + maximum (map depth c)

-- let f traverse through the tree
traverse :: (a -> b) -> Tree a -> [b]
traverse f (Leaf x) = [f x]
traverse f (Children ch) = concat $ map (traverse f) ch

main = do
	putExprLn $ Leaf 1
	putExprLn $ Children [Leaf 1, Leaf 2]

	let tree = Children [Leaf 1, Children [Leaf 2, Leaf 3]]

	let (Children ch) = tree
	let (x:xs) = ch

	-- child list
	putExprLn $ ch
	putExprLn $ x
	putExprLn $ xs

	putExprLn $ depth tree
	
	let logVisiting x = "Visit: " ++ (show x)
	let traverseLog = logVisiting `traverse` tree

	mapM_ putStrLn traverseLog
