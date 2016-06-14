module Problem91 where

import Control.Monad
import qualified Data.Set as S

type Coord = (Int, Int)

{-
-- original impl, rewritten to avoid some math calculation
-- and list overhead.
jump :: Int -> Coord -> [Coord]
jump n (x,y) = do
    (dx, dy) <- [ (1,2), (2,1) ]
    [sx,sy] <- replicateM 2 [1,-1]
    let newX = x + sx*dx
        newY = y + sy*dy
    guard $ newX >= 1 && newX <= n
         && newY >= 1 && newY <= n
    pure (newX, newY)
-}
jump :: Int -> Coord -> [Coord]
jump n (x,y) = do
    (dx, dy) <- [ (-1,-2)
                , (-1, 2)
                , ( 1,-2)
                , ( 1, 2)
                , (-2,-1)
                , (-2, 1)
                , ( 2,-1)
                , ( 2, 1)
                ]
    let newX = x + dx
        newY = y + dy
    guard $ newX >= 1 && newX <= n
         && newY >= 1 && newY <= n
    pure (newX, newY)

search :: Int -> S.Set Coord -> Coord -> [Coord] -> [ [Coord] ]
search n todo current path
    | S.null newTodo = pure (current:path)
    | otherwise = do
        next <- jump n current
        guard $ S.member next newTodo
        search n newTodo next (current:path)
  where
    newTodo = S.delete current todo

mkTodo :: Int -> S.Set Coord
mkTodo n = S.fromList [(x,y) | x <- as, y <- as]
  where
    as = [1..n]

knightsTo :: Int -> Coord -> [ [Coord] ]
knightsTo n target = search n (mkTodo n) target []

-- TODO: seems to never return. need to find out why
main :: IO ()
main = print (head $ knightsTo 8 (1,1))

{-

stack exec -- ghc -O2 Problem91.hs  -main-is Problem91

time:
real    7.74s
user    7.72s
sys     0.02s

-}
