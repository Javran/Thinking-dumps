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

search n todo current path = do
        let newTodo = S.delete current todo
            l = length (S.toList newTodo)
        if S.null newTodo
          then pure $ current : path
          else do
            next <- jump n current
            guard $ S.member next newTodo
            search n newTodo next (current:path)
