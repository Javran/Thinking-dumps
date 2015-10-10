module OCR
  ( convert
  ) where

import Data.List
import Data.List.Split
import Data.Char
import Data.Tuple

type OCRDigit = [String]

convert :: String -> String
convert = intercalate ","
        . (map . map) ocrDigitToChar
        . ocrChunks
        . lines

ocrTable :: [(OCRDigit, Int)]
ocrTable = map swap intToOcr
  where
    intToOcr = zip [0..] (ocrLineToChunks ocrs)
    ocrs =
        [ " _     _  _     _  _  _  _  _ "
        , "| |  | _| _||_||_ |_   ||_||_|"
        , "|_|  ||_  _|  | _||_|  ||_| _|"
        , "                              "
        ]

ocrDigitToChar :: OCRDigit -> Char
ocrDigitToChar od = case lookup od ocrTable of
    Nothing -> '?'
    Just v -> chr (v + ord '0')

ocrChunks :: [String] -> [ [ OCRDigit ] ]
ocrChunks ls = map ocrLineToChunks $ chunksOf 4 ls

ocrLineToChunks :: [String] -> [ OCRDigit ]
ocrLineToChunks = transpose . map (chunksOf 3)
