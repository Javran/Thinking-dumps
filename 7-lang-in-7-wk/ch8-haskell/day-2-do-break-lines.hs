import System.IO
import Control.Monad

import Utils

-- I'd like to treat my line breaker as some kind of printer

testFile = "day-2-do-break-lines-test.txt"

processLine :: String -> [String]
processLine = words

-- status definition:
-- (restWordList printedLines, currentLine)
data PrintStatus = 
	PrintStatus
		{ printedLines :: [String]
		, currentLine  :: String
		, widthLimit   :: Int
		} deriving (Show)

-- initialize printer
printInit :: Int -> PrintStatus
printInit widthLimit = PrintStatus
		{ printedLines = []
		, currentLine = []
		, widthLimit = widthLimit
		}

printWord printStatus word = 
	if (length $ currentLine printStatus) + (length word) + 1 <= (widthLimit printStatus)
		then
	   	-- simply append current word to the current line if possible
			PrintStatus
			{ printedLines = printedLines printStatus
			, currentLine = (currentLine printStatus) ++ " " ++ word
			, widthLimit = widthLimit printStatus
			}
		else
		-- time to switch to new line!
			PrintStatus
			{ printedLines = (printedLines printStatus) ++ [currentLine printStatus]
			, currentLine = ""
			, widthLimit = widthLimit printStatus
			}

-- break a long line into multiple lines with width given
breakLine :: Int -> String -> [String]
breakLine widthLimit rawLine = outputLines where
	outputLines = (printedLines printStatus) ++ [ currentLine printStatus ]
	printStatus = foldl printWord (printInit widthLimit) (words rawLine)

-- if you want to know how to deal with files in haskell
-- please refer to:
--     http://learnyouahaskell.com/input-and-output#files-and-streams
main = do
	hFile <- openFile testFile ReadMode
	content <- hGetContents hFile
	let fileLines = lines content

	putStrLn $ unlines $ join $ map (breakLine 80) fileLines 

	hClose hFile
