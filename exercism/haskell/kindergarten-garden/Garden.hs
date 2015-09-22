module Garden
  ( garden
  , defaultGarden
  , lookupPlants
  , Plant(..)
  ) where

data Plant
  = Grass
  | Clover
  | Radishes
  | Violets
    deriving (Eq, Show)

type Name = String

data Garden -- TODO

garden :: [Name] -> String -> Garden
garden ns raw = undefined

defaultNames :: [Name]
defaultNames = undefined

defaultGarden :: String -> Garden
defaultGarden = garden defaultNames

lookupPlants :: Name -> Garden -> [Plant]
lookupPlants = undefined
