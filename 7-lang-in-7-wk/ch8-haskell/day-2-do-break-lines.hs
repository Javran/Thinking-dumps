import System.IO
import Control.Monad

import Utils

-- I'd like to treat my line breaker as some kind of printer
--     by giving words one-by-one, the printer will finally produce the desired output

testFile = "day-2-do-break-lines-test.txt"

-- status definition:
-- (printedLines, currentLine, widthLimit)
data PrintStatus = 
	PrintStatus
		{ printedLines :: [String]
		, currentLine  :: String
		, widthLimit   :: Int
		} deriving (Show)

-- initialize printer with widthLimit
printInit :: Int -> PrintStatus
printInit widthLimit = PrintStatus
		{ printedLines = []
		, currentLine = []
		, widthLimit = widthLimit
		}

-- return new rest word list
printWord :: PrintStatus -> [String] -> (PrintStatus, [String])
printWord printStatus (word:restWordList) = 
	if currentLine printStatus == []
		then
			if length word <= sWidthLimit
				then
				-- nothing in current line, and word does not exceed line limit,
				--     simply printing the word will do
					(
						  PrintStatus
							{ printedLines = sPrintedLines
							, currentLine = word
							, widthLimit = sWidthLimit
							}
						, restWordList
					)
     				else
				-- the word cannot be contained in a single line
				--     we have no choice but break the word
				-- TODO: can string be broken at some position
				--     that we can get the first part & another part at same time
				--     instead of use `take` and `drop` ?
					(
						  PrintStatus
						  	{ printedLines = sPrintedLines
							, currentLine = take sWidthLimit word
							, widthLimit = sWidthLimit
							}
						, (drop sWidthLimit word):restWordList
					)
		else
			if (length sCurrentLine) + (length word) + 1 <= sWidthLimit
				then
			   	-- simply append current word to the current line if possible
					(
						  PrintStatus
							{ printedLines = sPrintedLines
							, currentLine = sCurrentLine ++ " " ++ word
							, widthLimit = sWidthLimit
							}
						, restWordList
					)
				else
				-- time to switch to new line!
					(
						  printNewline printStatus
						, word:restWordList
					)
     		where
		     	sPrintedLines = printedLines printStatus
		     	sCurrentLine = currentLine printStatus
			sWidthLimit = widthLimit printStatus

-- put current line into printed lines
printNewline :: PrintStatus -> PrintStatus
printNewline printStatus =
	PrintStatus
	{ printedLines = (printedLines printStatus) ++ [currentLine printStatus]
	, currentLine = ""
	, widthLimit = widthLimit printStatus
	}

-- tell a 'printer' print all words in a list
printAllWords :: PrintStatus -> [String] -> PrintStatus
-- all words' been printed, no more things to do
printAllWords printStatus [] = printStatus
-- attempt to feed the 'printer' with new word, recursively
printAllWords printStatus wordList = finalPrintStatus where
	(newPrintStatus, newRestWordList) = printWord printStatus wordList
	finalPrintStatus = printAllWords newPrintStatus newRestWordList

-- dump all printed texts from the 'printer'
printStatusToLines :: PrintStatus -> [String]
printStatusToLines printStatus = (printedLines printStatus) ++ [currentLine printStatus]

-- break a long line into multiple lines with width given
breakLine :: Int -> String -> [String]
breakLine widthLimit rawLine = outputLines where
	outputLines = printStatusToLines printStatus
	printStatus = printNewline $ printAllWords (printInit widthLimit) (words rawLine)

prettyOutput :: [String] -> IO ()
prettyOutput xs = do
	putStrLn "====== Output begin ======"
	putStr $ unlines xs
	putStrLn "======  Output end  ======"

-- format a number into string
--     and pad the string(using space) into a given length
numWidthFormat :: Int -> Int -> String
numWidthFormat n width = paddingSpaces ++ nStr where
	nStr = show n
	paddingSpaces = take (width-length nStr) $ repeat ' '

-- if you want to know how to deal with files in haskell
-- please refer to:
--     http://learnyouahaskell.com/input-and-output#files-and-streams
main = do
	putStrLn "Task #8: break long strings"

	hFile <- openFile testFile ReadMode
	content <- hGetContents hFile
	let fileLines = lines content
	let formattedLines = join $ map (breakLine 80) fileLines
	prettyOutput formattedLines

	putStrLn "Task #9: add line numbers"

	let maxLineLen = length $ show $ length formattedLines
	prettyOutput $ zipWith
		(\line content -> (numWidthFormat line maxLineLen) ++ " " ++ content)
		[1..]
		formattedLines

	hClose hFile

