{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , TupleSections
#-}
module Main where

import Types
import qualified VectorsAndPoints as VAP
import qualified TrailsAndPaths as TAP
import qualified Arrow as Arr

main :: IO ()
main = mainWith $ mconcat
    [ VAP.vapBundle
    , TAP.tapBundle
    , Arr.arrBundle
    ]
