{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , TupleSections
#-}
module TrailsAndPaths where

import Diagrams.Prelude
import Types

tapBundle :: Actioned Double
tapBundle = nest "tap" $ Actioned
    [ ("trailEx1", pure trailEx1)
    , ("trailEx2", pure trailEx2)
    ]

-- this can actually work without "strokeLine"
trailEx1 :: Diagram B
trailEx1 = strokeLine (fromOffsets [unitX, scale 2 unitY, scale 2 unitX])

trailEx2 :: Diagram B
trailEx2 = trailEx1 # rotate (negated ang)
  where
    -- not sure why, but I have to put type annotations for this to work..
    ang = angleBetweenDirs dir dirX
    trail :: Trail' Line V2 Double
    trail = fromOffsets [unitX, scale 2 unitY, scale 2 unitX]
    dir :: Direction V2 Double
    dir = direction (lineOffset trail)
    dirX :: Direction V2 Double
    dirX = direction unitX
