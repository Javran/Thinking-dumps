import Utils

stagger = (+2)
crawl = (+1)
treasureMap d = crawl $ stagger $ stagger d

data Position t = Position t
	deriving (Show)

staggerM = rtn.stagger
crawlM = rtn.crawl

-- actually `return` wrap things inside a monad
--     what you have learnt from book is slightly different from the real world
rtn x = Position x
(Position x) >>== f = Position newX where
	(Position newX) = f x

-- return (i.e. rtn) should put the number inside a Position
--     then bind functions to do things
treasureMapM pos = 
	rtn pos >>==
	staggerM >>==
	staggerM >>==
	crawlM

main = do
	putExprLn $ treasureMap 0

	let letTreasureMap d =
		let 
			d1 = stagger d
			d2 = stagger d1
			d3 = crawl d2
		in d3

	putExprLn $ letTreasureMap 0
	putExprLn $ treasureMapM 0
