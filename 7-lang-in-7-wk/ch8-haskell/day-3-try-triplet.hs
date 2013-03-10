import Utils

data Triplet a = Trio a a a
	deriving (Show)

main = do
	putExprLn $ Trio 1 2 3
	putExprLn $ Trio 1.0 2 3
	putExprLn $ Trio "A" "B" "C"

