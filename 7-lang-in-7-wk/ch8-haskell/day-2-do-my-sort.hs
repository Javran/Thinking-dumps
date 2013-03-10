import Data.List
import Utils

type Comparator a = a -> a -> Ordering
type SortByFunction a = Comparator a -> [a] -> [a]

-- http://en.wikipedia.org/wiki/Quick_sort
quickSortBy :: SortByFunction a
quickSortBy _ [] = []
quickSortBy cmp (x:xs) = (quickSortBy cmp leftPart) ++ [x] ++ (quickSortBy cmp rightPart) where
	leftPart  = [ e | e <- xs, e `cmp` x /= GT]
	rightPart = [ e | e <- xs, e `cmp` x == GT] 

-- http://en.wikipedia.org/wiki/Selection_sort
selectionSortBy :: SortByFunction a
selectionSortBy _ [] = []
selectionSortBy cmp xs = minElem : (selectionSortBy cmp restXs) where
	-- separate the min element from the list
	(minElem, restXs) = pickMin xs

	-- pick the minimum element from the list
	pickMin (x:xs) = foldl collectMin (x, []) xs where
		collectMin (curMin, restElements) curValue =
			if (curValue `cmp` curMin /= GT)
			   	-- curValue <= curMin
			   	--     use curValue and put back curMin
				then (curValue, curMin:restElements)
				-- curValue >  curMin
				--     else keep going
				else (curMin, curValue:restElements)

-- insert x into a sorted list using Comparator cmp
insertInto :: Comparator a -> a -> [a] -> [a]
insertInto cmp x list = leList ++ [x] ++ gtList where
	leList = [ le | le <- list, le `cmp` x /= GT] 
	gtList = [ gt | gt <- list, gt `cmp` x == GT]

-- http://en.wikipedia.org/wiki/Insertion_sort
insertionSortBy :: SortByFunction a
insertionSortBy cmp = foldr (insertInto cmp) []

testSortBy sortByFunc sortName cmp inList =
	if (sortByFunc cmp inList) == (sortBy cmp inList)
		then do
			putStr sortName
			putStrLn ": Test passed"
		else do
			putStr sortName
			putStrLn ": Test failed"

testSort sB sN = testSortBy sB sN compare

main = do
	let testCase = [1,9,2,8,3,7,4,6,5]
	let sortFunctions = 
		[ (quickSortBy, 	"quick sort    ")
		, (selectionSortBy, 	"selection sort")
		, (insertionSortBy, 	"insertion sort") ]

	putStrLn "Task #1: sort function:"
	putStrLn "The result should be: "
	putExprLn $ sort testCase
	mapM_ (\ (x,n) -> testSort x n testCase) sortFunctions

	let invCompare = flip compare
	
	putStrLn "Task #2: sortBy function:"
	putStrLn "The result should be: "
	putExprLn $ sortBy invCompare testCase
	mapM_ (\ (x,n) -> testSortBy x n invCompare testCase) sortFunctions
