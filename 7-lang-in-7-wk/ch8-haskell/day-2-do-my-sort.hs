import Utils

quickSortBy :: (Ord a) => (a -> a -> Ordering) -> [a] -> [a]
quickSortBy _ [] = []
quickSortBy cmp (x:xs) = (quickSortBy cmp leftPart) ++ [x] ++ (quickSortBy cmp rightPart) where
	leftPart  = [ e | e <- xs, e `cmp` x /= GT]
	rightPart = [ e | e <- xs, e `cmp` x == GT] 

quickSort = quickSortBy compare

main = do
	putExprLn $ quickSort [1,9,2,8,3,7,4,6,5]
