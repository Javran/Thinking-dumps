module Problem99 where

import qualified Data.Array.IArray as Arr
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

import Data.Maybe
import Data.Char
import Data.Ix
import Control.Monad
import Control.Arrow

type Words = IM.IntMap [String]
type Coord = (Int,Int)

-- site
data Dir = DV | DH deriving Show -- vertical or horizontal
-- Site <length> <starting coord> <direction>
data Site = Site Int Coord Dir deriving Show

-- at most 2 sites on the same coord (one v and one h)
data Framework = FW
  { fwDimension :: (Int, Int)
  , fwSites :: [Site]
  , fwHints :: M.Map Coord Char
  } deriving (Show)

data Crossword = CW Words Framework deriving (Show)
type Rect = Arr.Array Coord (Maybe Char)

crossWordFromFile :: FilePath -> IO Crossword
crossWordFromFile fp = parse . lines <$> readFile fp
  where
    parse :: [String] -> Crossword
    parse xs = CW (mkWords ws) (mkFramework cs)
      where
        (ws,_:xs1) = break null xs
        (cs,_) = break null xs1

mkWords :: [String] -> Words
mkWords = foldr update IM.empty
  where
    update w = IM.alter ins lw
      where
        lw = length w
        ins Nothing = Just [w]
        ins (Just xs) = Just (w:xs)

mkFramework :: [String] -> Framework
mkFramework [] = error "empty input"
mkFramework xs@(y:_)
    | null y = error "first line empty"
    | otherwise = FW (nRows,nCols) sites hints
  where
    paddedXs = map (take nCols . (++ repeat ' ')) xs

    nCols = maximum (map length xs)
    nRows = length xs

    allCoords = [ (r,c)
                | r <- [1..nRows]
                , c <- [1..nCols]]
    rect :: Arr.Array Coord Char
    rect =
        Arr.array
          ((1,1),(nRows,nCols))
          (zip allCoords
               (concat paddedXs))
    rBounds = Arr.bounds rect

    findDirSite :: Coord -> Dir -> Maybe Site
    findDirSite coord dir = do
        let prevCoord = prev coord
        guard $
            -- not empty cell
            rect Arr.! coord /= ' '
            -- previous cell out of bound or is empty
         && (not (inRange rBounds prevCoord)
            || rect Arr.! prevCoord == ' ')
        let candidates = iterate next (next coord)
            site = coord
                   : takeWhile
                       (\coord' ->
                           inRange rBounds coord'
                        && rect Arr.! coord' /= ' ')
                       candidates
            lSite = length site
        guard $ lSite >= 2
        pure (Site lSite coord dir)
      where
        (prev, next) = case dir of
            DH -> (second pred, second succ)
            DV -> (first pred, first succ)
    findSites :: Coord -> [Site]
    findSites c = mapMaybe (findDirSite c) [DH,DV]
    sites = concatMap findSites allCoords
    hints =
        M.fromList
      . filter (isAsciiUpper . snd)
      . map (\c -> (c, rect Arr.! c))
      $ allCoords

solvePuzzle :: Crossword -> Maybe Rect
solvePuzzle (CW ws (FW (nRows,nCols) sites hints)) = undefined
  where
    -- initial rectangle containing hints
    initRect :: Rect
    initRect =
          Arr.accum
            (\_ new -> Just new)
            (Arr.listArray ((1,1),(nRows,nCols)) (repeat Nothing))
            (M.toList hints)
    sitesByLen :: IM.IntMap [Site]
    sitesByLen =
        IM.map ($ [])
      . IM.fromListWith (.)
      . map (\s@(Site l _ _) -> (l,(s:)) )
      $ sites
