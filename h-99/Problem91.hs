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

search n todo current path
    | S.null todo = pure $ current : path
    | otherwise = do
        let newTodo = S.delete current todo
        next <- jump n current
        guard $ S.member next newTodo
        search n newTodo next (current:path)
-- try: (very slow, might not work)
-- > let xs = S.fromList [(x,y) | x <- [1..5], y <- [1..5]]
-- > head $ search 5 xs S.empty (1,1) []
