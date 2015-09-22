module Garden
  ( garden
  , defaultGarden
  , lookupPlants
  , Plant(..)
  ) where

import Data.Maybe
import Data.List.Split
import Data.List
import qualified Data.Map.Strict as M

data Plant
  = Grass
  | Clover
  | Radishes
  | Violets
    deriving (Eq, Show)

type Name = String

type Garden = M.Map Name [Plant]

toPlant :: Char -> Plant
toPlant c = case c of
    'G' -> Grass
    'C' -> Clover
    'R' -> Radishes
    'V' -> Violets
    _ -> error "unknown plant type"

garden :: [Name] -> String -> Garden
garden ns raw = M.fromList (zip (sort ns) twoCols)
  where
    -- "ABCD\nEFGH" ->
    -- [ [A,B,C,D]
    -- , [E,F,G,H]
    -- ]
    rows :: [[Plant]]
    rows = (map . map) toPlant . lines $ raw
    -- step 1:
    -- [ [[A,B],[C,D]]
    -- , [[E,F],[G,H]]
    -- ]
    -- step 2:
    -- [ [[A,B],[E,F]]
    -- , [[C,D],[G,H]]
    -- ]
    -- step 3:
    -- [ [A,B,E,F]
    -- , [C,D,G,H]
    -- ]
    twoCols :: [[Plant]]
    twoCols = map concat -- step 3
            . transpose -- step 2
            . map (chunksOf 2) -- step 1
            $ rows

defaultNames :: [Name]
defaultNames = words
    "Alice Bob Charlie David \
    \Eve Fred Ginny Harriet \
    \Ileana Joseph Kincaid Larry "

defaultGarden :: String -> Garden
defaultGarden = garden defaultNames

lookupPlants :: Name -> Garden -> [Plant]
lookupPlants n = fromJust . M.lookup n
