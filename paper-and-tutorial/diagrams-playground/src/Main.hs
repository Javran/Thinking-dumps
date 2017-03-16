{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , TypeFamilies
#-}
module Main where

-- http://projects.haskell.org/diagrams/doc/vector.html

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Vector (e)

ex1 :: Diagram B
ex1 = fromOffsets (concat (replicate 5 [V2 1 1, V2 1 (-1)]))

ex2 :: Diagram B
ex2 = foldMap (\vec -> node # translate vec) vecs
  where
    vecs = [ 5 *^ e (r @@ rad) | r <- take 7 [tau/4, tau/4-step ..] ]
    step = tau / 2 / 6
    node = circle 1 # fc blue

ex3 :: Diagram B
ex3 = mconcat $ do
    r <- [step, tau/10 + step .. tau]
    x <- [1..3]
    [ fromOffsets [x *^ e (r + step * (x-1) @@ rad)] ]
  where
    step = tau/30

vTriangle :: V2 Double -> V2 Double -> Diagram B
vTriangle va vb = fromOffsets [va, negated va ^+^ vb , negated vb]

parallelogram :: V2 Double -> V2 Double -> Diagram B
parallelogram v1 v2 = mconcat
    [ fromOffsets [v1] # lc blue
    , fromOffsets [v1] # lc blue # translate v2 # dashingG [0.1,0.1] 0
    , fromOffsets [v2] # lc red
    , fromOffsets [v2] # lc red # translate v1 # dashingG [0.1,0.1] 0
    , fromOffsets [v1 ^+^ v2] # lc purple
    ]

main :: IO ()
main = mainWith ex2 -- (parallelogram (unitX # rotateBy (1/120)) (unitX # rotateBy (1/8)))
