{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , TupleSections
#-}
module TrailsAndPaths where

import Diagrams.Prelude
import Control.Arrow
import Types

tapBundle :: Actioned Double
tapBundle = nest "tap" $ Actioned
    [ ("ex1", pure ex1)
    , ("ex2", pure ex2)
    , ("ex3", pure ex3)
    ]

-- this can actually work without "strokeLine"
ex1 :: Diagram B
ex1 = strokeLine (fromOffsets [unitX, scale 2 unitY, scale 2 unitX])

ex2 :: Diagram B
ex2 = ex1 # rotate (negated ang)
  where
    -- not sure why, but I have to put type annotations for this to work..
    ang = angleBetweenDirs dir dirX
    trail :: Trail' Line V2 Double
    trail = fromOffsets [unitX, scale 2 unitY, scale 2 unitX]
    dir :: Direction V2 Double
    dir = direction (lineOffset trail)
    dirX :: Direction V2 Double
    dirX = direction unitX

ex3 :: Diagram B
ex3 = fromVertices (origin : take 9 (map p2 coords'))
  where
    coords' = (0,1) : (1,0) : (map . first) succ coords'
