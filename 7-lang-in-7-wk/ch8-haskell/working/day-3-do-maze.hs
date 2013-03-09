import Data.Ix

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
		|| (node1 x, node2 x) == (node2 y, node1 y) 

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

-- returns all reachable neighbors of a node in maze
neighbors :: Maze -> Node -> [Node]
neighbors maze node = reachableNeighbors where
	allNeighbors = [ Node (col node + x) (row node + y) | (x,y) <- directions]
	validNeighbors = filter (`inside` maze) allNeighbors
	reachableNeighbors = [ nNode | nNode <- validNeighbors, not $ (Wall node nNode) `elem` (walls maze) ]

-- let's make a test case
-- +-+-+-+
-- | |   |
-- + + + +
-- |   | |
-- +-+-+-+
-- maze: width = 3, height = 2
--     2 walls: (1,1)-(2,1) and (2,2)-(3,2)
testMaze = Maze 
	{ width 	= 3
	, height 	= 2
	, walls 	= 
		[ Wall 
			{ node1 = Node 1 1
			, node2 = Node 2 1}
		, Wall
			{ node1 = Node 2 2
			, node2 = Node 3 2}
		]
	}

-- TODO: pretty print for maze

main = do
	let testNodes = [ Node x y | x <- [1..3], y <- [1..2] ]
	mapM_ (putExprLn.(neighbors testMaze)) testNodes
