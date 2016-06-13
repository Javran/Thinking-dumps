module Problem91 where

import Control.Monad
import qualified Data.Set as S
import Debug.Trace

type Coord = (Int, Int)

jump :: Int -> Coord -> [Coord]
jump n (x,y) = do
    (dx, dy) <- [ (1,2), (2,1) ]
    [sx,sy] <- replicateM 2 [1,-1]
    let newX = x + sx*dx
        newY = y + sy*dy
    guard $ newX >= 1 && newX <= n
         && newY >= 1 && newY <= n
    pure (newX, newY)

search n todo visited current path
    | S.null todo = pure $ current : path
    | otherwise = do
        let newVisited = S.insert current visited
        next <- jump n current
        guard $ S.notMember next newVisited
        traceM (show (length path))
        search n (S.delete current todo) newVisited next (current:path)
-- try: (very slow, might not work)
-- > head $ search 8 (S.delete (1,1) xs) S.empty (1,1) []

