import Utils

-- key should be a string
type HashKey = String

-- value should be either value or hashtable
data HashValue a = Value a | ChildTable (HashTable a)
	deriving (Show)

type HashTable a = [(HashKey, HashValue a)]

-- lookup the value of k in hashtable tbl
lookupHashTable :: HashTable a -> HashKey -> Maybe (HashValue a)
lookupHashTable tbl k =
	if length findResult == 0
		then Nothing
		else Just $ snd $ head findResult
	where
		findResult = filter ((==k).fst) tbl

main = do
	putStrLn "Task #1: write a hashtable lookup function"

	let monadTable = ChildTable [
		  ("IO", 	Value "IO computations")
		, ("State", 	Value "Stateful computations")
		, ("List", 	Value "Possibility")]

	let testTable = [
		  ("language", Value "Haskell")
		, ("monads", monadTable)]

	putStrLn "The hash table is:"
	putExprLn $ testTable
