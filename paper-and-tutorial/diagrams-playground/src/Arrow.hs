{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleContexts
  , FlexibleInstances
  , TypeFamilies
#-}
module Arrow where

-- http://projects.haskell.org/diagrams/doc/arrow.html
import Data.Maybe
import Diagrams.Prelude
import Diagrams.TwoD.Vector
import Types

arrBundle :: Actioned Double
arrBundle = nest "arr" $ Actioned
    [ ("ex1_1", pure ex1_1)
    , ("ex2_1", pure ex2_1)
    , ("ex2_2", pure ex2_2)
    , ("ex2_3", pure ex2_3)
    , ("example", pure arrowsExample)
    , ("ex3_1", pure ex3_1)
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

genEx2 :: ArrowOpts Double -> Diagram B
genEx2 arrOpt = (sDot <> eDot <> xArr <> sq) # centerXY # pad 1.1
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
    xArr = arrowBetween' arrOpt sPt ePt

ex2_1 :: Diagram B
ex2_1 = genEx2 arrOpt
  where
    arrOpt = with
         & headLength .~ veryLarge
         & arrowHead .~ noHead
         & arrowTail .~ spike'

ex2_2 :: Diagram B
ex2_2 = genEx2 arrOpt
  where
    shaft =  arc xDir (-1/4 @@ turn)
    arrOpt = with
         & arrowShaft .~ shaft
         & arrowHead .~ tri
         & arrowTail .~ tri'

ex2_3 :: Diagram B
ex2_3 = genEx2 arrOpt
  where
    shaft =  arc xDir (1/4 @@ turn)
    arrOpt = with
         & arrowShaft .~ shaft
         & arrowHead .~ tri
         & arrowTail .~ tri'


arrowsExample :: Diagram B
arrowsExample = ( field # translateY 0.05
                  <> ( square 3.5 # fc whitesmoke # lwG 0.02 # alignBL))
                # scaleX 2
  where
    locs = [(x, y) | x <- [0.1, 0.3 .. 3.25], y <- [0.1, 0.3 .. 3.25]]
    -- create a list of points where the vectors will be place.
    points = map p2 locs
    -- The function to use to create the vector field.
    vectorField (x, y) = r2 (sin (y + 1), sin (x + 1))
    arrows = map arrowAtPoint locs
    arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
      where
        vf   = vectorField (x, y)
        m    = norm $ vectorField (x, y)
        -- Head size is a function of the length of the vector
        -- as are tail size and shaft length.
        hs   = 0.08 * m
        sW   = 0.015 * m
        sL   = 0.01 + 0.1 * m
        opts = (with & arrowHead .~ tri & headLength .~ global hs & shaftStyle %~ lwG sW)
    field   = position $ zip points arrows

-- TODO: perhaps visualize gravitational field, with multiple object of large mass in it.
-- the formula is F = G*m_1*m_2/r^2, let's fix m_1, and make G*m_1=c a constant, making it F = c*m_2 / r^2
-- first we start with just one large mass object, and then adjust to support multiple of them.
ex3_1 :: Diagram B
ex3_1 = (field # centerXY)
        <> (square 12 # fc whitesmoke # lwG 0.02)
  where
    coords1D = [-5,-5+0.5 .. 5]
    locs = (,) <$> coords1D <*> coords1D
    -- create a list of points where the vectors will be place.
    points = map p2 locs
    field = position $ zipWith
              (\pos mVV -> case mVV of
                   Nothing -> (pos, mempty)
                   Just vv -> (pos, arrowV vv)) points forceField
    -- this is assuming there is a big mess right at the origin
    -- TODO: making force vectors, rescale and render it.
    coordToForce :: (Double, Double) -> Maybe (V2 Double)
    coordToForce (x,y) = if dist == 0 then Nothing else Just (g *^ (vv ^/ dist))
      where
        vv = r2 (-x,-y)
        dist = sqrt (x*x + y*y)
        m2 = 1000
        c = 1.0
        g = c*m2 / dist

    forceField = (fmap . fmap) (^/ maxDist) forceField1
      where
        forceField1 = map coordToForce locs
        calcDist v = v ^. _r
        maxDist = maximum (catMaybes (fmap (fmap calcDist) forceField1))
