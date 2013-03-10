import Data.Ix
import Data.List
import Control.Monad
import System.IO

import Utils

-- coordinate system: 2d coordinate
--     one based,
--     (1,1) for the up-leftmost node,
--     (w,h) for the down-rightmost node,

data Node = Node
	{ col 		:: Int
	, row 		:: Int}
	deriving (Eq, Ord)

instance Show Node where
	show n = "(" ++ (show $ col n) ++ "," ++ (show $ row n) ++ ")"

data Wall = Wall
	{ node1 	:: Node
	, node2 	:: Node}
	deriving (Show)

nodeInOrd :: Wall -> (Node, Node)
nodeInOrd (Wall n1 n2) = 
	if n1 <= n2
		then (n1,n2)
		else (n2,n1)

instance Eq Wall where
	x == y = (nodeInOrd x) == (nodeInOrd y)

instance Ord Wall where
	compare x y = compare (nodeInOrd x) (nodeInOrd y)

-- remove duplicate/invalid walls
pruneWalls :: Maze -> Maze
pruneWalls maze = Maze w h prunedXs where
	(Maze w h xs) = maze
	prunedXs = filter validWall $ map head $ group $ sort xs
	validWall (Wall a b) = all (`inside` maze) [a,b]

data Maze = Maze
	{ width 	:: Int
	, height 	:: Int
	, walls 	:: [Wall] }
	deriving (Show, Eq)

data MazeProblem = MazeProblem
	{ start 	:: Node
	, end 		:: Node }
	deriving (Eq)

instance Show MazeProblem where
	show (MazeProblem start end) =
		"Problem: from " ++ (show start) ++ " to " ++ (show end)

newtype MazeSolution = Solution
	{ getSolution   :: [Node] }

instance Show MazeSolution where
	show (Solution (x:xs)) = (show x) ++ concatMap ((" -> " ++ ).show) xs 

instance Eq MazeSolution where
	(Solution a) == (Solution b) = a == b

instance Ord MazeSolution where
	compare (Solution a) (Solution b) = compare a b

insertHead :: Node -> MazeSolution -> MazeSolution
insertHead x (Solution xs) = Solution (x:xs) 

-- check if the node is inside maze
inside :: Node -> Maze -> Bool
node `inside` maze = 
	inRange (1, width maze) (col node) && 
	inRange (1, height maze) (row node)

-- judge if two neighboring node is connected
isNeighborConnected :: Maze -> Node -> Node -> Bool
isNeighborConnected maze n1 n2 =
	-- if n1 n2 are both inside the maze 
	if all (`inside` maze) [n1,n2]
		-- if Wall n1 n2 is an element of walls
		--     they are not connected
		then not $ (Wall n1 n2) `elem` (walls maze)
		-- n1 or n2 is outside the maze, they are not collected
		else False

-- returns all reachable neighbors of a node in maze
neighbors :: Maze -> Node -> [Node]
neighbors maze node = reachableNeighbors where
	directions = 
		[ ( 0,-1)
		, ( 0, 1)
		, (-1, 0)
		, ( 1, 0)]
	allNeighbors = [ Node (col node + x) (row node + y) | (x,y) <- directions]
	validNeighbors = filter (`inside` maze) allNeighbors
	reachableNeighbors = [ nNode | nNode <- validNeighbors, not $ (Wall node nNode) `elem` (walls maze) ]

-- return lines that pretty prints the maze when outputed line-by-line
prettyLinesMaze :: Maze -> [String]
prettyLinesMaze maze = header : body where
	mWidth = width maze
	mHeight = height maze
	header = foldl (++) "+" $ replicate mWidth "-+"
	body = concat $ do -- flatten results
		line <- [1..mHeight]
		let cellInLine = [ Node x line | x <- [1..mWidth] ]
		-- now for each cell we can verify the connectivity of bottom side & right side	
		-- returns a tuple: (bottomSide, rightSide),
		--     bottomSide :: Bool
		--     rightSide :: Bool
		let cellInfos = do
			(Node x y) <- cellInLine
			let bottomSide = not $ isNeighborConnected maze (Node x y) (Node x (y+1))
			let rightSide = not $ isNeighborConnected maze (Node x y) (Node (x+1) y)
			return (bottomSide, rightSide)
		-- `toCellStrings` covers any cellInfo to a tuple: (cell1, cell2) where 
		--     cell1 = " |" or "  "
		--     cell2 = "-+" or " +"
		let toCellStrings (bottomSide, rightSide) =
			( if rightSide then " |" else "  "
			, if bottomSide then "-+" else " +") 
		let cellStrings = map toCellStrings cellInfos
		-- reduce strings, and decorate with appropriate chars
		let line1 = ('|' :) $ concatMap fst cellStrings
		let line2 = ('+' :) $ concatMap snd cellStrings
		return [line1, line2]

-- solve the maze, return all solutions
solve :: Maze -> MazeProblem -> [MazeSolution]
solve maze (MazeProblem start end) = solveMask maze (MazeProblem start end) []

-- solve the maze, with some nodes masked
--     this is only a naive DFS
--     we can speed up by providing which nodes have been searched
--     given that we only need to search one time for each node to find the best solutions
solveMask :: Maze -> MazeProblem -> [Node] -> [MazeSolution]
solveMask maze (MazeProblem start end) visitedNodes 
	| start == end = [Solution [end]]
	| otherwise = concat $ do
		-- pick up one possible move
		nextNode <- neighbors maze start
		-- ensure not to visit nodes that have been visited
		guard $ not $ nextNode `elem` visitedNodes
		
		-- get all possible moves from nextNode to end
		let subSolutions = solveMask maze (MazeProblem nextNode end) (start:visitedNodes)
		let currentSolutions = map (insertHead start) subSolutions
		return currentSolutions

-- filter out results that are not the shortest
optimizeResult :: [MazeSolution] -> [MazeSolution]
optimizeResult solutions = filter ((==bestLen).solutionLength) solutions where
	bestLen = minimum $ map solutionLength solutions
	solutionLength (Solution xs) = length xs


parseMaze :: Int -> Int -> [String] -> Maybe Maze
parseMaze w h (header:rawMaze) = do
	-- verify and drop the first line
	let rightHeader = foldl (++) "+" $ replicate w "-+"
	-- rawMaze with indices
	let rawMazeI = zip [1..] rawMaze
	if header /= rightHeader
		then Nothing
		else if any (\ (ind, line) -> if odd ind
				-- verify and remove the first col
				then head line == '+' 
				else head line == '|') rawMazeI
			then Nothing
			else do
				walls <- parseCellSides $ map tail rawMaze
				-- TODO: remove duplicate/invalid walls
				return $ Maze w h walls
	where
		-- after removing the first line and leading char from rawLine
		--     let's parse cell connection info into walls
		parseCellSides :: [String] -> Maybe [Wall]
		parseCellSides rawMaze = do
			-- group every 2 lines together
			-- and add indices for each line
			let walls = map (uncurry parseCellLine) $ zip [1..] $ splitToPairs rawMaze
			if any (== Nothing) walls
				then Nothing
				else return $ concat $ map (\(Just x) -> x) walls

		parseCellLine :: Int -> (String, String) -> Maybe [Wall]
		-- sample input: 
		--     2 (" |   |"
		--      , "-+ +-+")
	   	-- will be broken into: 
		-- [ (1, (< |>, <-+>))
		-- , (2, (<  >, < +>))
		-- , (3, (< |>, <-+>))]
		-- "<ab>" ::= ('a', 'b')
		parseCellLine ind (line1, line2) = do
			let splitedLine1 = splitToPairs line1
			let splitedLine2 = splitToPairs line2
			let parseResult = map (\(x,l1,l2) -> parseCell x ind (l1,l2)) $ 
					zip3 [1..] splitedLine1 splitedLine2
			if any (== Nothing) parseResult
				then Nothing
				else return $ concat $ map (\(Just x) -> x) parseResult

		parseCell :: Int -> Int -> ((Char, Char), (Char, Char)) -> Maybe [Wall]
		parseCell x y ((a,b), (c,d)) =
			-- a b
			-- c d
			if a /= ' ' || d /= '+' 
				|| (not $ b `elem` " |") 
				|| (not $ c `elem` " -")
				then Nothing
				else do
					let node = Node x y
					let rightWall = if b == ' '
								then []
								else [Wall node (Node (x+1) y)]
					let bottomWall = if c == ' '
							    	then []
								else [Wall node (Node x (y+1))]
					return $ concat [rightWall, bottomWall]

		splitToPairs :: [a] -> [(a, a)]
		splitToPairs [] = []
		splitToPairs (l:r:xs) = (l,r):(splitToPairs xs) 

parseProblemList :: [String] -> Maybe [MazeProblem]
parseProblemList xs = do
	let problemList = map parseProblem xs
	if any (== Nothing) problemList
		then Nothing
		else return $ map (\(Just x) -> x) problemList

parseProblem :: String -> Maybe MazeProblem
parseProblem rawProblem = do
	let [p,rawStart,rawEnd] = words rawProblem

	let
		(startX, startY) = 
			read rawStart :: (Int, Int)
		(endX, endY) = 
			read rawEnd :: (Int, Int)

	if p /= "Problem"
		then Nothing
		else return $ MazeProblem 
				(Node startX startY)
				(Node endX endY)

-- load test cases from file
loadTestCase :: FilePath -> IO (Maybe (Maze, [MazeProblem]))
loadTestCase file = do
	putStrLn $ "Loading test case from file: " ++ file
	handle <- openFile file ReadMode
	contents <- hGetContents handle	
	let rawLines = lines contents
	-- read width & height for maze
	let [width, height] = map read $ words $ head $ rawLines :: [Int]
	let (beginStr, endStr) = ("-- BEGIN", "-- END")	
	let rawMaze = tail $ takeWhile (/= endStr) $ dropWhile (/= beginStr) rawLines
	let rawProblems = tail $ dropWhile (/=endStr) rawLines
	-- verify raw data length
	if not $ all ((== (width*2+1)).length) rawMaze
		then return Nothing
		else 
			if length rawMaze /= (height*2+1)
				then return Nothing
				else do
					let parseResult = do
						maybeMaze <- parseMaze width height rawMaze
						maybeProblems <- parseProblemList rawProblems
						return (pruneWalls maybeMaze, maybeProblems)
					if parseResult == Nothing
						then return Nothing
						else do
							let (Just (maze, problems)) = parseResult
							putStrLn "Maze loaded:"
							mapM_ putStrLn $ prettyLinesMaze maze
							putStrLn "Problem list:"
							mapM_ putExprLn problems
							return parseResult

doTestCaseFile file = do
	parseResult <- loadTestCase file
	if parseResult == Nothing
		then return ()
		else do
			let (Just (maze, problems)) = parseResult
			putStrLn "Start problem solving ..."
			mapM_ (solveMazeIO maze) problems

solveMazeIO :: Maze -> MazeProblem -> IO [MazeSolution]
solveMazeIO maze problem = do
	putExprLn problem
	let solutions = optimizeResult $ solve maze problem
	putStr "Shortest solution(s): "
	if length solutions == 0
		then putStrLn "No solution"
		else putStrLn ""


	mapM_ putExprLn solutions
	return solutions

main = do
	-- let's make a test case
	-- +-+-+-+
	-- | | | |
	-- + + + +
	-- |   | |
	-- +-+-+-+
	-- maze: width = 3, height = 2
	--     3 walls:
	--         (1,1)-(2,1)
	--         (2,2)-(3,2)
	--         (2,1)-(3,1)
	let testMaze = Maze 
		{ width 	= 3
		, height 	= 2
		, walls 	= 
			[ Wall ( Node 1 1 ) ( Node 2 1 )
			, Wall ( Node 2 2 ) ( Node 3 2 )
			, Wall ( Node 2 1 ) ( Node 3 1 )
			]
		}
	let testNodes = [ Node x y | x <- [1..3], y <- [1..2] ]
	-- testNodes' connectivity
	let testConnectivityIO n = do
		putStr "Node: "
		putExprLn n
		putStr "reachable neighbors: "
		putExprLn $ neighbors testMaze n

	putStrLn "Task #2: represent a maze"
		
	mapM_ testConnectivityIO testNodes
	mapM_ putStrLn $ prettyLinesMaze testMaze

	putStrLn "Task #3: solve the maze"

	mapM_ doTestCaseFile 
		[ "day-3-do-maze-in-1.txt"
		, "day-3-do-maze-in-2.txt"]
