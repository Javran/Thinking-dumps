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

-- add line number to a list of String
decorateWithLineNumber :: [String] -> [String]
decorateWithLineNumber xs = 
	zipWith
		(\line content -> (numWidthFormat line maxLineLen) ++ " " ++ content)
		[1..]
		xs
	where
		maxLineLen = length $ show $ length xs 

data JustifyType = LeftJustify | RightJustify | FullJustify
	deriving (Enum, Show)

justifyLine :: Int -> JustifyType -> String -> String
justifyLine width jType content =
	if length content >= width 
		then
		-- nothing need to do
			content
		else
			case jType of
			     LeftJustify  	-> content ++ padding
			     RightJustify 	-> padding ++ content
			     -- TODO: implement full justify
			     FullJustify 	-> fullJustifier width content
			where
				padding = take (width - length content) $ repeat ' '

justifyLines w jType = map $ justifyLine w jType

fullJustifier :: Int -> String -> String
fullJustifier widthLimit originalStr =
	if length wordList <= 1
		then
		-- simply padding spaces will do
			newStr ++ separatorPool
		else
		-- word count >= 2
		--     we need to insert spaces between words
			justifyWordList wordList
	where
		newStr = join wordList
		wordList = words originalStr
 
		separator = ' '
		-- separatorPool: separators to be inserted
		separatorPool = take (widthLimit - (length newStr)) (repeat separator)
		separatorPoolLen = length separatorPool
				 
		justifyWordList (x:xs) =
		-- it's guaranteed that length wordList > 1
		-- the justification is based on one equation:
		--
		--         (x `div` y) * y + (x `mod` y) == x
		--
		--     so each separator group should at least have a length of (x `div` y)
		--     and there should now be a gap that has width (x `mod` y)
		--     to fill that gap, the first (x `mod` y) separator is assigned with an extra separator
		--     because of that, the word list should be indexed
		--     so we can distinguish different separator groups.
		--
		-- * separator group is defined as consecutive separators (i.e. spaces) between two words
			foldl (\acc i -> acc ++ uncurry justifyWord i) x indexedXs
			where
				-- bind words with indices
				indexedXs = zip [1..] xs
 
				-- pad word with leading spaces
				-- the number of spaces is inferred from its index
				justifyWord ind word = (take spaceLen (repeat separator)) ++ word
					where
						spaceLen =
							  separatorPoolLen `div` (length xs)
							+ if ind <= separatorPoolLen `mod` (length xs) then 1 else 0

-- if you want to know how to deal with files in haskell
-- please refer to:
--     http://learnyouahaskell.com/input-and-output#files-and-streams

-- related articles about laziness and `seq`:
-- http://stackoverflow.com/questions/9707190/time-cost-of-haskell-seq-operator
-- http://www.haskell.org/haskellwiki/Seq
-- http://www.haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:seq

main = do
	putStrLn "Task #8: break long strings"

	hFile <- openFile testFile ReadMode
	content <- hGetContents hFile
	content `seq` return ()
	hClose hFile

	let fileLines = lines content
	let formattedLines = join $ map (breakLine 80) fileLines
	prettyOutput formattedLines

	putStrLn "Task #9: add line numbers"
	prettyOutput $ decorateWithLineNumber formattedLines

	putStrLn "Task #10: justify lines"
	mapM_
		(\x -> do
			putStrLn $ "Justify type: " ++ show x
			prettyOutput $ decorateWithLineNumber $ justifyLines 80 x formattedLines
			putStrLn "")
		[LeftJustify, RightJustify, FullJustify]

