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

-- pretty print a hashtable with given indent level
prettyPrintHashTable :: (Show a) => Int -> HashTable a -> [String]
prettyPrintHashTable indentLevel table =
	map (indentStr ++ ) keyValueShowList
	where
		keyValueShowList = concat $ map prettyPrintKeyValuePair table
		indentStr = concat $ replicate indentLevel "\t"

-- pretty print a key-value pair
prettyPrintKeyValuePair :: (Show a) => (HashKey, HashValue a) -> [String]
prettyPrintKeyValuePair (key,value) = (prettyPrintKey key) : (prettyPrintValue value)

prettyPrintKey :: HashKey => String
prettyPrintKey k = "K: " ++ k

prettyPrintValue :: (Show a) => HashValue a -> [String]
prettyPrintValue (Value v) = ["V: " ++ (show v)]
prettyPrintValue (ChildTable table) = "V: HashTable" : printedTable where
	printedTable = prettyPrintHashTable 1 table

prettyPrintLookupIO :: (Show a) => HashTable a -> HashKey -> IO ()
prettyPrintLookupIO table key = do
	putStr $ "Lookup key " ++ (show key) ++ " in hashtable ... "
	lookupResultPrintIO $ lookupHashTable table key

prettyPrintLookupDeepIO :: (Show a) => HashTable a -> [HashKey] -> IO ()
prettyPrintLookupDeepIO table keys = do
	putStrLn "Lookup recursively with key list: "
	putExprLn keys
	lookupResultPrintIO $ lookupDeepHashTable table keys

lookupResultPrintIO :: (Show a) => Maybe (HashValue a) -> IO ()
lookupResultPrintIO Nothing = putStrLn "Failed"
lookupResultPrintIO (Just v) = do
	putStrLn "OK:"
	mapM_ putStrLn $ prettyPrintValue v

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
	mapM_ putStrLn $ prettyPrintHashTable 0 testTable
	
	prettyPrintLookupIO testTable "language"

	mapM_ (prettyPrintLookupDeepIO testTable) [
		  ["language"]
		-- haskell
		, [ "monads", "more"]
		-- we'll find `moreMonad`
		, ["monads", "more", "Maybe"]
		-- Data.Maybe
		, ["error"]
		-- Nothing
		, ["monads"]
		-- `monadTable`
		, ["monads", "IO", "oh"]]
		-- Nothing
