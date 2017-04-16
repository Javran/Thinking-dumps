{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
#-}
module Arrow where

-- http://projects.haskell.org/diagrams/doc/arrow.html

import Diagrams.Prelude
import Types

arrBundle :: Actioned Double
arrBundle = nest "arr" $ Actioned
    [ ("ex1_1", pure ex1_1)
    , ("ex2_1", pure ex2_1)
    ]

ex1_1 :: Diagram B
ex1_1 = (sDot <> eDot <> xArr <> circle 1) # centerXY # pad 1.1
  where
    spot = circle 0.02 # lw none
    ptStart = origin .+^ (unitX # rotateBy (1/8))
    ptEnd = origin .+^ (unitX # rotateBy (1/2))
    sDot = spot # fc blue # moveTo ptStart
    eDot = spot # fc red # moveTo ptEnd
    xArr = arrowBetween' (with & headLength .~ veryLarge) ptStart ptEnd

ex2_1 :: Diagram B
ex2_1 = (sDot <> eDot <> xArr <> sq) # centerXY # pad 1.1
  where
    -- square by default has its origin at center
    -- we can do "moveTo (p2 (2,2))" to move it so that
    -- its bottom left corner becomes the origin.
    -- there's a more elegant and descriptive way: using alignBL
    sq = square 4 # alignBL
    spot = circle 0.02 # lw none
    sDot = spot # fc blue # moveTo sPt
    eDot = spot # fc red # moveTo ePt
    sPt = p2 (1,1)
    ePt = p2 (3,3)
    xArr = arrowBetween'
        (with
         & headLength .~ veryLarge
         & arrowHead .~ noHead
         & arrowTail .~ spike) sPt ePt
