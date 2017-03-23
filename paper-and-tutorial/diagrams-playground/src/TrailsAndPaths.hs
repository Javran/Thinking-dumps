{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , TupleSections
#-}
module TrailsAndPaths where

-- http://projects.haskell.org/diagrams/doc/paths.html

import Diagrams.Prelude
import Control.Arrow
import Types

tapBundle :: Actioned Double
tapBundle = nest "tap" $ Actioned
    [ ("ex1_1", pure ex1_1)
    , ("ex1_2", pure ex1_2)
    , ("ex1_3", pure ex1_3)
    , ("ex1_4", pure ex1_4)
      -- alternative ex4 using "pentagon" function
    , ("ex1_4alt", pure ex1_4Alt)
    , ("ex1_5", pure ex1_5)
    , ("ex2_1", pure ex2_1)
    ]

-- this can actually work without "strokeLine"
ex1_1 :: Diagram B
ex1_1 = strokeLine (fromOffsets [unitX, scale 2 unitY, scale 2 unitX])

ex1_2 :: Diagram B
ex1_2 = ex1_1 # rotate (negated ang)
  where
    -- not sure why, but I have to put type annotations for this to work..
    ang = angleBetweenDirs dir dirX
    trail :: Trail' Line V2 Double
    trail = fromOffsets [unitX, scale 2 unitY, scale 2 unitX]
    dir :: Direction V2 Double
    dir = direction (lineOffset trail)
    dirX :: Direction V2 Double
    dirX = direction unitX

ex1_3 :: Diagram B
ex1_3 = fromVertices (origin : take 9 (map p2 coords'))
  where
    coords' = (0,1) : (1,0) : (map . first) succ coords'

ex1_4 :: Diagram B
ex1_4 = fromOffsets (take 5 xs)
  where
    xs = unitX : map (rotateBy (1/5)) xs

ex1_4Alt :: Diagram B
ex1_4Alt = pentagon 1

{-

"pentagon" does not specify how segments are arranged,
we know with "onLineSegments" it's just a matter of locate the segment in
question and remove it, but it still takes some trial and error to figure this out.

-}
ex1_5 :: Diagram B
ex1_5 = strokeLine $ onLineSegments tail (pentagon 1)

ex2_1 :: Diagram B
ex2_1 = strokeLine $ dg `mappend` dg
  where
    dg = onLineSegments init (pentagon 1)
