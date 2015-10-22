module Queens
  ( boardString
  , canAttack
  ) where

type Coord = (Int, Int)

boardString :: Maybe Coord -> Maybe Coord -> String
boardString wCoord bCoord =
    unlines (map (unwords . map printCell) coords)
  where
    cs = [0 :: Int ..7]
    coords = [ [(row,col) | col <- cs] | row <- cs ]
    printCell coord
        | Just coord == wCoord = "W"
        | Just coord == bCoord = "B"
        | otherwise = "_"

canAttack :: Coord -> Coord -> Bool
canAttack (wX,wY) (bX,bY) =
       wX == bX -- same row
    || wY == bY -- same col
    || wX - wY == bX - bY -- share diagonal
    || wX + wY == bX + bY -- share diagonal
