module Problem91 where

import Control.Monad
import qualified Data.Set as S
import Data.List

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
        let -- all possible next moves
            nexts = jump n current
            -- sort by the number of ways one node can be expanded in ascending order.
            -- by doing so we make sure nodes with fewer future moves are explored first
            -- this could improve performance by reducing branching factor.
            sortedNexts = sortOn countMoves nexts
            countMoves :: Coord -> Int
            countMoves c = length (filter (`S.member` newTodo) (jump n c))
        next <- sortedNexts
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

main :: IO ()
main = print (head $ knightsTo 8 (1,1))

{-

see: https://wiki.haskell.org/The_Knights_Tour

* if the search function is implemented naively without specifying
  searching order of moves, the performance is bad, because
  it is likely that moves with more branches are explored first,
  resulting in a huge search space.

* to fix the problem above, we can sort candidates by number of immediate valid moves
  right after it and then explore candidates that have fewer immediate valid moves first.
  by doing so the branching factor is reduced

-}
