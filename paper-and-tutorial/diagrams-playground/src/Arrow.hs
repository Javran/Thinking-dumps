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
