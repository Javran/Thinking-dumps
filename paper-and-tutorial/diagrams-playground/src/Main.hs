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
import Data.Foldable
import Data.List
import Data.Ord
import qualified Data.Set as S

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
    [ drawV v1 # lc blue
    , drawV v1 # lc blue # translate v2 # dashing'
    , drawV v2 # lc red
    , drawV v2 # lc red # translate v1 # dashing'
    , drawV (v1 ^+^ v2) # lc purple
    ]
  where
    drawV v = fromOffsets [v]
    dashing' = dashingG [0.1,0.1] 0

circleGrid :: Diagram B
circleGrid = mconcat $ do
    x <- [-15 .. 15 :: Int]
    y <- [-15 .. 15]
    let p = p2 (x,y)
        (xD,yD) = (fromIntegral x, fromIntegral y)
        pCenter = origin
        qDist = qd p pCenter
        cir = circle 1 # fc (if qDist <= 15*15 then yellow else purple)
    pure (cir # translate (r2 (xD+xD,yD+yD)))

main :: IO ()
main = mainWith
    [ ("ex1", ex1)
    , ("ex2", ex2)
    , ("ex3", ex3)
    , ("vTriangle", vTriangle unitX (unitX # rotateBy (1/8)))
    , ("parallelogram", parallelogram (unitX # rotateBy (1/120)) (unitX # rotateBy (1/8)))
    , ("circlegrid", circleGrid)
    ]

{-
TODO:

- impl Graham scan
- generate random points for testing
- render diagram
- extend to record steps
- render diagram (with steps)

-}

grahamScan :: S.Set (P2 Int) -> [P2 Int]
grahamScan pSet
    | S.size pSet < 3 = error "insufficient points"
    | otherwise = reverse $ go [startPoint2,startPoint] visitList
  where
    startPoint = minimumBy cmp' pSet
    toDbl p = let (x,y) = unp2 p in p2 (fromIntegral x, fromIntegral y :: Double)
    cmp' pa pb =
        let ((xa,ya),(xb,yb)) = (unp2 pa, unp2 pb)
        in (ya `compare` yb) <> (xa `compare` xb)
    visitList :: [P2 Int]
    (startPoint2 : visitList) =
          sortBy (comparing (\p -> signedAngleBetween (toDbl p .-. toDbl startPoint) unitX))
        . S.toList
        $ S.delete startPoint pSet
    go :: [P2 Int] {- current set of convex points -}
       -> [P2 Int] {- a list to be visited -}
       -> [P2 Int]
    go vs [] = vs
    go vs@(pt2:pt1:vs') vList@(ptCur:vList') =
        let va = pt2 .-. pt1
            vb = ptCur .-. pt2
        in if leftTurn va vb
             then go (ptCur:vs) vList'
             else go (pt1:vs') vList
