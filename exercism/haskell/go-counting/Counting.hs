module Counting
  ( Color(..)
  , territories
  , territoryFor
  ) where

import qualified Data.Set as S
import Data.Array
import Data.List
import Data.Foldable
import Data.Maybe

data Color
  = Black
  | White
    deriving (Eq, Ord, Show)

type Coord = (Int, Int)
type Board = [ [] Char ] -- shut up hlint
type Owner = Maybe Color
type CellArray = Array Coord Owner

-- one corollary from the rule is that:
-- + two neighboring empty cells must be of the same side.
--   otherwise these two cells cannot be counted as their sides
--   in the first place.
-- so one solution is to first clustering neigboring cells to figure out
-- empty intersections, and then we can determine which side owns the
-- territory.

-- TODO: determine cluster owner
territories :: Board -> [(S.Set Coord, Owner)]
territories bd = map tagWithOwner (clusterCells emptyCells S.empty [])
  where
    ca = toCellArray bd
    -- get coords of empty cells
    emptyCells = map fst $ filter ((== Nothing). snd) (assocs ca)

    neighbor (x,y) = filter (inRange (bounds ca))
                            [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

    clusterCells :: [Coord] -- cells to be visited
                 -> S.Set Coord -- current set of cells
                 -> [S.Set Coord] -- current set of clusters
                 -> [S.Set Coord] -- return
    clusterCells [] curCells clusters =
        if S.null curCells
          then clusters
          else curCells : clusters
    clusterCells todos@(x:xs) curCells clusters =
        -- if the current set of coords is empty
        -- then we are not ready to expand the set,
        if S.null curCells
          then
            -- pick one element to form the initial set
            clusterCells xs (S.singleton x) clusters
          else
            let neighbors = S.fromList . concatMap neighbor . toList $ curCells
                todoSet = S.fromList todos
                found = neighbors `S.intersection` todoSet
                remaining = toList (todoSet `S.difference` found)
            in if S.null found
                 then clusterCells todos S.empty (curCells : clusters)
                 else clusterCells remaining (found `S.union` curCells) clusters
    tagWithOwner coordSet = (coordSet, owner)
      where
        neighborOwners :: S.Set Color
        neighborOwners = S.fromList
                       . mapMaybe (ca !)
                       . concatMap neighbor
                       . S.toList
                       $ coordSet
        owner
            | neighborOwners == S.singleton Black = Just Black
            | neighborOwners == S.singleton White = Just White
            | otherwise = Nothing

territoryFor :: Board -> Coord -> Maybe (S.Set Coord, Owner)
territoryFor b coord = find (\(cs,_) -> coord `S.member` cs) t
  where
    t = territories b

toCellArray :: Board -> CellArray
toCellArray b = listArray ((1,1),(cols,rows))
                          (map toOwner (concat (transpose b)))
  where
    rows = length b
    cols = length (head b)
    toOwner ch = case ch of
        'W' -> Just White
        'B' -> Just Black
        ' ' -> Nothing
        _ -> error "invalid board"
