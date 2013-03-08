import Utils

-- key should be a string
type HashKey = String

-- value should be either value or hashtable
data HashValue a = Value a | ChildTable (HashTable a)
	deriving (Show)

type HashTable a = [(HashKey, HashValue a)]

-- check if a HashValue is Value
isValue :: HashValue a -> Bool
isValue (Value _) = True
isValue _ = False

-- lookup the value of k in hashtable
lookupHashTable :: HashTable a -> HashKey -> Maybe (HashValue a)
lookupHashTable table k =
	if length findResult == 0
		then Nothing
		else Just $ snd $ head findResult
	where
		findResult = filter ((==k).fst) table

-- give a list of key level-by-level
--     lookup the value in a given hashtable recursively
lookupDeepHashTable :: HashTable a -> [HashKey] -> Maybe (HashValue a)
-- if key list is empty, nothing to do
lookupDeepHashTable table [] = Nothing
-- only one key in the list, simply lookup it in the table
lookupDeepHashTable table [k] = lookupHashTable table k
lookupDeepHashTable table (k:restKey) = do
	-- lookup the value
	hashValue <- lookupHashTable table k
	-- the line below will be evaluated
	--     only if we want to know what is `newTable`
	let ChildTable (newTable) = hashValue

	-- since key list has more than one element
	--     we need to confirm the value is a sub-hashtable
	if not $ isValue hashValue
		then lookupDeepHashTable newTable restKey
		else Nothing

main = do
	putStrLn "Task #1: write a hashtable lookup function"

	let moreMonad = ChildTable [
		  ("Reader", 	Value "Control.Monad.Reader")
		, ("Writer", 	Value "Control.Monad.Writer")
		, ("Maybe", 	Value "Data.Maybe")]

	let monadTable = ChildTable [
		  ("IO", 	Value "IO computations")
		, ("State", 	Value "Stateful computations")
		, ("List", 	Value "Possibility")
		, ("more", 	moreMonad)]

	let testTable = [
		  ("language", Value "Haskell")
		, ("monads", monadTable)]

	putStrLn "The hash table is:"
	putExprLn $ testTable
	
	putExprLn $ lookupHashTable testTable "language"

	putExprLn $ lookupDeepHashTable testTable ["language"]
	-- haskell
	putExprLn $ lookupDeepHashTable testTable ["monads", "more"]
	-- we'll find `moreMonad`
	putExprLn $ lookupDeepHashTable testTable ["monads", "more", "Maybe"]
	-- Data.Maybe
	putExprLn $ lookupDeepHashTable testTable ["error"]
	-- Nothing
	putExprLn $ lookupDeepHashTable testTable ["monads"]
	-- `monadTable`
	putExprLn $ lookupDeepHashTable testTable ["monads", "IO", "oh"]
	-- Nothing
