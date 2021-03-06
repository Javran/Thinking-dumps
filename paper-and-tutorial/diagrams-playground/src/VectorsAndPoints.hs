{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
  , TupleSections
#-}
module VectorsAndPoints where

-- http://projects.haskell.org/diagrams/doc/vector.html

import Data.List.Split (chunksOf)

import Diagrams.TwoD.Vector (e)
import Data.Foldable
import Control.Monad.Random
import Data.Ord
import qualified Data.Set as S
import System.IO.Unsafe
import qualified Data.DList as DL
import Control.Arrow

import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Merge as V
import Control.Monad.Writer hiding ((<>))

import Diagrams.Prelude
import Types

vapBundle :: Actioned Double
vapBundle = nest "vap" $ Actioned
    [ ("ex1", pure ex1)
    , ("ex2", pure ex2)
    , ("ex3", pure ex3)
    , ("vTriangle", pure $ vTriangle unitX (unitX # rotateBy (1/8)))
    , ( "parallelogram"
      , pure $ parallelogram (unitX # rotateBy (1/120)) (unitX # rotateBy (1/8)))
    , ("circlegrid", pure circleGrid)
    , ("grahamscan",
       do
           let gen :: IO (S.Set (P2 Int))
               gen = do
                   ptCount <- getRandomR (10,30)
                   (S.fromList . map p2)
                       <$> replicateM ptCount
                       ((,)
                        <$> getRandomR (-20,20)
                        <*> getRandomR (-20,20))
           dgs <- renderGrahamScanSteps <$> gen
           pure (mkGrids dgs))]

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

-- has to be stable
sortViaVector :: Ord a => [a] -> [a]
sortViaVector xs = unsafePerformIO $ do
    vec <- V.unsafeThaw (V.fromList xs)
    V.sort vec
    V.toList <$> V.unsafeFreeze vec

sortByViaVector :: (a -> a -> Ordering) -> [a] -> [a]
sortByViaVector f xs = unsafePerformIO $ do
    vec <- V.unsafeThaw (V.fromList xs)
    V.sortBy f vec
    V.toList <$> V.unsafeFreeze vec

renderGrahamScanSteps
    :: S.Set (P2 Int)
    -> [Diagram B]
renderGrahamScanSteps pSet =
    -- from DList
      toList
    . fmap (\es -> foldMap applyColor es `atop` baseDg)
    $ accumulated
  where
    applyColor (x,c) = x # lc c
    (baseDg, accumulated) = runWriter (renderedGrahamScanSteps pSet)

{-

For internal use only. please call "renderGrahamScanSteps"

-}
renderedGrahamScanSteps
    :: S.Set (P2 Int)
    -> Writer (DL.DList [(Diagram B, Colour Double)]) (Diagram B)
renderedGrahamScanSteps pSet = do
    {-

    every "addDiagram" writes down a list of edges that have just enough info
    to construct the edge part of the diagram.
    for this list of edges, each element is a (color-less) diagram paired with a color
    because how "diagrams" handles attributes, "<some diagram> # lc <color1> # lc <color2>"
    will always result in "color1" being used, which is not what we want, so
    we workaround this by keeping track of the color info outside of diagram
    and apply it only when we actually start rendering.

    -}

    {-

    for color representation, a newly added one would have color green,
    and when we are removing an edge, the edge in question is colored red,
    otherwise blue is used for those edges currently in set (not newly added)

    -}
    let addDiagram = tell . DL.singleton
        (GAPick startPoint1:GAPick startPoint2:gsLogRemained) = gsLog
    -- first diagram containing nothing but vertices
    addDiagram []
    let -- creates a line between pt1 and pt2
        lineBetween pt1 pt2 = toDbl pt1 ~~ toDbl pt2
        diagram1 = lineBetween startPoint1 startPoint2
        initEdgeDiagramList = [(diagram1, green)]
    addDiagram initEdgeDiagramList -- adding first edge
    -- scan the log and recover steps that Graham scan has actually done
    finalEdgeDiagramList <- fix (\self prevEdgeDiagramList prevStack curLog -> case curLog of
        [] -> pure prevEdgeDiagramList
        (action:remainedLog) -> case action of
            GAPick nextPt -> do
                let curEdgeDiagram = lineBetween (head prevStack) nextPt
                    curEdgeDiagramList =
                        (curEdgeDiagram, green) : markPrevDone prevEdgeDiagramList
                addDiagram curEdgeDiagramList
                self curEdgeDiagramList (nextPt:prevStack) remainedLog
            GADrop -> do
                addDiagram (changePrevColor red prevEdgeDiagramList)
                self (tail prevEdgeDiagramList) (tail prevStack) remainedLog
        ) initEdgeDiagramList [startPoint2,startPoint1] gsLogRemained
    let (hdPt1, hdPt2) = head edges
        finalDiagram1 = (lineBetween hdPt1 hdPt2, green) : markPrevDone finalEdgeDiagramList
    addDiagram finalDiagram1
    addDiagram (markPrevDone finalDiagram1)
    pure allVertices
  where
    changePrevColor c ((hd,_) : xs) = (hd,c) : xs
    changePrevColor _ [] = []
    markPrevDone = changePrevColor blue
    toDbl p = let (x,y) = unp2 p in p2 (fromIntegral x, fromIntegral y :: Double)
    vertexDg = circle 0.1 # fc black
    -- a diagram with only all vertices
    allVertices :: Diagram B
    allVertices = foldMap (\pt -> vertexDg # translate (pt .-. origin)) (S.map toDbl pSet)
    convexPts :: [P2 Int]
    (convexPts, gsLog) = second toList $ runWriter (grahamScanW pSet)
    edges =
        (last convexPts, head convexPts)
        : zip convexPts (tail convexPts)

{-

for keeping track of how Graham scan picks or removes vertices
as its iteration goes.

there are only two kinds of actions that we care:

- GAPick is picking a new vertex into the set
- GADrop is removing the last one picked

-}

data GrahamAction a
  = GAPick a
  | GADrop
    deriving (Show)

{-

impl of Graham scan,
- takes a set of unique points and return a list of them
  representing the convex.
- the result is a list, vertices are picked in that order
  to form the convex
- what Writer monad has recorded is the history of adding and removing
  vertices as Graham scan goes, with this we can recover the exact step
  and visualize it.

-}
grahamScanW
    :: S.Set (P2 Int)
    -> Writer (DL.DList (GrahamAction (P2 Int))) [P2 Int]
grahamScanW pSet
    | S.size pSet < 3 = error "insufficient points"
    | otherwise = reverse <$> do
        tell (DL.singleton (GAPick startPoint))
        tell (DL.singleton (GAPick startPoint2))
        go [startPoint2,startPoint] visitList
  where
    startPoint = minimumBy cmp' pSet
    toDbl p = let (x,y) = unp2 p in p2 (fromIntegral x, fromIntegral y :: Double)
    cmp' pa pb =
        let ((xa,ya),(xb,yb)) = (unp2 pa, unp2 pb)
        in (ya `compare` yb) <> (xa `compare` xb)
    visitList :: [P2 Int]
    (startPoint2 : visitList) =
          sortByViaVector
            (comparing (\p -> signedAngleBetween (toDbl p .-. toDbl startPoint) unitX))
        . S.toList
        $ S.delete startPoint pSet
    go :: [P2 Int] {- current set of convex points -}
       -> [P2 Int] {- a list to be visited -}
       -> Writer (DL.DList (GrahamAction (P2 Int))) [P2 Int]
    go vs [] = pure vs
    go vs@(pt2:pt1:vs') vList@(ptCur:vList') =
        let va = pt2 .-. pt1
            vb = ptCur .-. pt2
        in -- we are not testing just left turn, but non-right turns, straight line counts.
           if leftTurn va vb || not (leftTurn vb va)
             then tell (DL.singleton (GAPick ptCur)) >> go (ptCur:vs) vList'
             else tell (DL.singleton GADrop) >> go (pt1:vs') vList
    {-
      the following two cases are unreachable:
      - the guard at the beginning (i.e. "S.size pSet < 3") should have ensured that we
        have sufficient number of points
      - we have gave two initial points in "vs", so the only way that the size of "vs"
        ever goes down is through failing the "non-right turn" test, but since the list
        is sorted by ascending angles, this could never happen.
     -}
    go [] _ = error "unreachable (empty)"
    go [_] _ = error "unreachable (singleton)"

mkGrids :: [Diagram B] -> Diagram B
mkGrids xs = vsep 10 (map (hsep 10) ys)
  where
    ys = chunksOf w xs
    l = length xs
    w = ceiling (sqrt (fromIntegral l) :: Double) :: Int
