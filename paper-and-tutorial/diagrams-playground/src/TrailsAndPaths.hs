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
import Diagrams.Direction
import Diagrams.Trail
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
    , ("ex2_2", pure (strokeLine ex2_2))
    , ("ex2_3", pure ex2_3)
    , ("ex2_3alt", pure ex2_3Alt)
    , ("ex3_1", pure ex3_1)
    , ("ex3_2", pure ex3_2)
    , ("ex4_1", pure ex4_1)
    , ("ex4_2", pure ex4_2)
    , ("ex4_3", pure ex4_3)
    , ("ex5_1", pure ex5_1)
    , ("ex5_2", pure ex5_2)
    , ("ex6_1", pure ex6_1)
    , ("ex6_2", pure ex6_2)
    , ("ex6_3", pure ex6_3)
    , ("ex6_4", pure ex6_4)
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

-- ex2_2 :: Diagram B
ex2_2 = mconcat $ iterateN 5 (rotateBy (1/5)) dg
  where
    dg = onLineSegments init (pentagon 1)

ex2_3 :: Diagram B
ex2_3 = strokeLine $ dg3 <> reverseLine (reflectX dg3)
  where
    dg1 = fromOffsets [unitX, unitY # rotateBy (1/12), unitX # rotateBy (1/6) ^* 2]
    dg2 = dg1 <> reverseLine (reflectX dg1)
    dg3 = dg2
          <> fromOffsets [unitX]
          <> rotateBy (-1/3) (reverseLine dg2)
          <> fromOffsets [rotateBy (1/12) unitY]
          <> rotateBy (1/3) dg2
          <> fromOffsets [unitX # rotateBy (1/6)]
          <> rotateBy (-1/3) (reverseLine dg2)
          <> fromOffsets [unitX # rotateBy (1/6)]
          <> dg1

-- TODO: this is not quite working...
ex2_3Alt :: Diagram B
ex2_3Alt = strokeLine (mkLines (fromOffsets [unitX]))
  where
    dg1 = fromOffsets [unitX, unitY # rotateBy (1/12), unitX # rotateBy (1/6) ^* 2] # rotateBy (-1/6) # reflectY
    mkLines basicDg = f $ basicDg <> reverseLine dg1' <> rotateBy (1/6) dg'
      where
        dg' = reflectY basicDg
        dg1' = rotateBy (-1/6) dg'
        f x = x <> reverseLine (reflectX x)

ex3_1 :: Diagram B
ex3_1 = strokeLoop (pentagon 1) # fc blue

-- should be just lines with no color filled
ex3_2 :: Diagram B
ex3_2 = strokeLine (pentagon 1) # fc blue

ex4_1 :: Diagram B
ex4_1 = strokeLoop (glueLine ex2_2) # fc green

ex4_2 :: Diagram B
ex4_2 = strokeLoop (glueLine l) # fc red
  where
    l = fromOffsets (concat (replicate 5 [unitY,unitX]) ++ [unit_Y ^* 5, unit_X ^* 5])

ex4_3 :: Diagram B
ex4_3 = strokeLoop (glueLine (mconcat (take 5 (iterate (rotateBy (-1/5)) dg)))) # fc blue
  where
    -- some work on paper can mathematically show that it's 0.3 (turn)
    dg = fromOffsets [unitY]
        <> arc (dir unit_X) ((-1/2) @@ turn)
        <> fromOffsets [unit_Y]
        <> arc (dir unit_X) (0.3 @@ turn)

ex5_1 :: Diagram B
ex5_1 = strokeLoop (closeLine (fromOffsets [r2 (1,3),unitX ^* 3, r2(1,-3)]))

ex5_2 :: Diagram B
ex5_2 = strokeLoop (closeLine $ l1 <> stimes (10 :: Int) (l1 <> l2) <> l2) # fc yellow
  where
    l1 = fromOffsets [r2 (1,5)]
    l2 = fromOffsets [r2 (1,-5)]

ex6_1 :: Diagram B
ex6_1 = mconcat (map strokeLocTrail ts')
  where
    -- without "mapLoc" we will rotate the line about an universal origin,
    -- which leads to the effect of rotating whole picture.
    -- not very sure about how "mapLoc" works, but I think this ensures
    -- that a located trail is rotated about a point relative to itself.
    ts' :: [Located (Trail V2 Double)]
    ts' = map (mapLoc (rotateBy (1/24))) ts
    ts :: [Located (Trail V2 Double)]
    ts = explodeTrail (heptagon 1)

ex6_2 :: Diagram B
ex6_2 = mconcat (zipWith (\v f -> strokeLocTrail v # f # lw 20) sqs' m)
  where
    m = lc red : lc blue : m
    -- after we are satisfied with the trail, we explode it again,
    -- but this time make it located.
    sqs' :: [Located (Trail V2 Double)]
    sqs' = explodeTrail (sqs `at` origin)
    -- to prevent exploded trails from being fixed automatically,
    -- we use type to explicitly exclude location info
    sqs :: Trail V2 Double
    sqs = mconcat (concatMap (replicate 4) $ explodeTrail (square 1))

ex6_3 :: Diagram B
ex6_3 = mconcat paths # lw 20
  where
    -- from the looks of it:
    -- + we should obtain points through `regPoly`
    -- + connect them using `star`,
    -- + `pathTrails` will take things apart
    -- + then each part can be colored individually
    points :: [Point V2 Double]
    points = regPoly (5 * 6) 1

    d1 = star (StarSkip 5) points

    colors = red : orange : yellow : blue : green : colors
    paths :: [Diagram B]
    paths = zipWith (\d c -> strokeLocTrail d # lc c) (pathTrails d1) colors

ex6_4 :: Diagram B
ex6_4 = strokePath path # fillRule EvenOdd # fc blue
  where
    path :: Path V2 Double
    path = atPoints pts (circle 3 : repeat (circle 1))
    pts :: [Point V2 Double]
    pts = origin : origin : regPoly 6 2
