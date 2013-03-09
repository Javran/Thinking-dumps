import Data.Ix
import Data.List
import Control.Monad

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

instance Eq Wall where
	x == y =
		   (node1 x, node2 x) == (node1 y, node2 y)
		--                            ^        ^
		|| (node1 x, node2 x) == (node2 y, node1 y) 
		--                            ^        ^

data Maze = Maze
	{ width 	:: Int
	, height 	:: Int
	, walls 	:: [Wall] }
	deriving (Show)

data MazeProblem = MazeProblem
	{ start 	:: Node
	, end 		:: Node }
	deriving (Show)

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
		return [ line1, line2]

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
		putExprLn n
		putStr "reachable neighbors: "
		putExprLn $ neighbors testMaze n

	putStrLn "Task #2: represent a maze"
		
	mapM_ testConnectivityIO testNodes
	mapM_ putStrLn $ prettyLinesMaze testMaze

	putStrLn "Task #3: solve the maze"
	putExprLn $ solve testMaze $ MazeProblem (Node 1 1) (Node 2 1)
	putExprLn $ solve testMaze $ MazeProblem (Node 1 1) (Node 3 1)

	let maze = Maze 5 5 []
	let sol = solve maze $ MazeProblem (Node 1 1) (Node 5 5)
	putExprLn $ length $ optimizeResult sol
