module Beer
  ( verse
  , sing
  , song
  )
where

import Data.Char
import Data.Function
import Text.Printf

-- TODO: looks awful, will go back after migration is done.

song :: String
song = sing 99 0

sing :: Int -> Int -> String
sing x y = init $ (unlines . map verse) [x, x -1 .. y]

verse :: Int -> String
verse x =
  unlines
    [ firstLine x
    , secondLine x
    ]

-- | for sake of readability, should be invisible outside the module
_onTheWall, _end :: Bool
_onTheWall = True
_end = False

-- | construct "<n> bottle(s) of beer (on the wall)"
xBottleOfBeer :: Int -> Bool -> String
n `xBottleOfBeer` otw = unwords [nStr, bottles, "of beer", otwS]
  where
    nStr = if n == 0 then "no more" else show n
    bottles = if n == 1 then "bottle" else "bottles"
    otwS = if otw then "on the wall" else ""

{-# ANN makeSentence "HLint: ignore Eta reduce" #-}

-- | construct a sentence from two parts
makeSentence :: String -> String -> String
makeSentence (fstH : fstT) sndL =
  -- create the sentence on stripped arguments
  (printf "%s, %s." `on` stripR) fstL' sndL
  where
    -- take care of the first char in sentences
    fstL' = toUpper fstH : fstT
    -- might have tailing zeros, remove it.
    stripR = reverse . dropWhile isSpace . reverse

firstLine :: Int -> String
firstLine x =
  makeSentence
    (x `xBottleOfBeer` _onTheWall)
    (x `xBottleOfBeer` _end)

secondLine :: Int -> String
secondLine 0 =
  makeSentence
    "Go to the store and buy some more"
    (99 `xBottleOfBeer` _onTheWall)
secondLine 1 =
  makeSentence
    "Take it down and pass it around"
    (0 `xBottleOfBeer` _onTheWall)
secondLine x =
  makeSentence
    "Take one down and pass it around"
    ((x -1) `xBottleOfBeer` _onTheWall)
