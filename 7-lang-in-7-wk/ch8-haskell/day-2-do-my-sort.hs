import Data.List
import Utils

type SortByFunction a = (a -> a -> Ordering) -> [a] -> [a]

-- http://en.wikipedia.org/wiki/Quick_sort
quickSortBy :: (Ord a) => SortByFunction a
quickSortBy _ [] = []
quickSortBy cmp (x:xs) = (quickSortBy cmp leftPart) ++ [x] ++ (quickSortBy cmp rightPart) where
	leftPart  = [ e | e <- xs, e `cmp` x /= GT]
	rightPart = [ e | e <- xs, e `cmp` x == GT] 

-- http://en.wikipedia.org/wiki/Selection_sort
selectionSortBy :: (Ord a) => SortByFunction a
selectionSortBy _ [] = []
selectionSortBy cmp xs = minElem : (selectionSortBy cmp restXs) where
	-- separate the min element from the list
	(minElem, restXs) = pickMin xs

	-- pick the minimum element from the list
	pickMin (x:xs) = foldl collectMin (x, []) xs where
		collectMin (curMin, restElements) curValue =
			if (curValue < curMin)
			   	-- use curValue and put back curMin
				then (curValue, curMin:restElements)
				-- else keep going
				else (curMin, curValue:restElements)


testSortBy sortByFunc cmp inList =
	if (sortByFunc cmp inList) == (sortBy cmp inList)
		then putStrLn "Test passed"
		else putStrLn "Test failed"

main = do
	let testCase = [1,9,2,8,3,7,4,6,5]
	testSortBy selectionSortBy compare testCase
