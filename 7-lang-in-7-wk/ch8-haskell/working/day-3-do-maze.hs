import Data.Ix
import Control.Monad

import Utils

-- coordinate system: 2d coordinate
--     one based,
--     (1,1) for the up-leftmost node,
--     (w,h) for the down-rightmost node,

data Node = Node
	{ col 		:: Int
	, row 		:: Int}
	deriving (Eq, Show)
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

directions = 
	[ ( 0,-1)
	, ( 0, 1)
	, (-1, 0)
	, ( 1, 0)]

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
		let line1 = ('|' :) $ foldl1 (++) $ map fst cellStrings
		let line2 = ('+' :) $ foldl1 (++) $ map snd cellStrings
		return [ line1, line2]

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
