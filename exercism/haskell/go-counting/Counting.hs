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

territories :: Board -> [(S.Set Coord, Owner)]
territories bd = map tagWithOwner (clusterCells (S.fromList emptyCells) S.empty [])
  where
    ca = toCellArray bd
    -- get coords of empty cells
    emptyCells = map fst $ filter ((== Nothing). snd) (assocs ca)

    neighbor (x,y) = filter (inRange (bounds ca))
                            [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]

    clusterCells :: S.Set Coord -- cells to be visited
                 -> S.Set Coord -- current set of cells
                 -> [S.Set Coord] -- current set of clusters
                 -> [S.Set Coord] -- return
    clusterCells todoSet curCells clusters = case S.minView todoSet of
        Nothing -> -- no more cells to check
            if S.null curCells
              then clusters
              else curCells : clusters
        Just (x,xs) ->
            -- if the current set of coords is empty
            -- then we are not ready to expand the set,
            if S.null curCells
              then
                -- pick one element to form the initial set
                -- note that S.maxView can also be used,
                -- we just want to pick one element from the set
                -- how it's picked is irrelevant.
                clusterCells xs (S.singleton x) clusters
              else
                let neighbors = S.fromList . concatMap neighbor . toList $ curCells
                    found = neighbors `S.intersection` todoSet
                    remaining = todoSet `S.difference` found
                in if S.null found
                     then clusterCells todoSet S.empty (curCells : clusters)
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
