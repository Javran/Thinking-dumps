{-
  answer to the question in exercise 4.39:
  changing the order of constraints will not change the result
  by it does have effects on the search space.

  To show that the result is not affected, we should consider two facts:
  * changing the position of any constraint has nothing to do with
    the order of enumerating all solutions
  * changing the position of any constraint will neither cause a previous solution
    considered valid, nor a previous invalid solution considered valid.
    (guaranteed by the commutativity of "logical and")

  To show that the search space will be impacted by changing the position
  of a constraint, see the definitiion of `demoSearchSpaceEffect`
  in the following code.

  `demoCondOrder` shows that the order of constraint matters
-}

import Data.List

import Control.Monad.List
import Control.Monad.Writer
import Control.Arrow

distinct :: Eq e => [e] -> Bool
distinct xs = length xs == length ys
    where
        ys = nub xs

puzzleSolutions1 :: ListT (Writer [Int]) [(String, Int)]
puzzleSolutions1 = do
    let floors = ListT (return [1..5])
    let record x = lift $ tell [x]

    baker    <- floors
    cooper   <- floors
    fletcher <- floors
    miller   <- floors
    smith    <- floors

    -- live on different floors
    guard (distinct [baker, cooper, fletcher, miller, smith])
    record 1

    -- Baker does not live on the top floor
    guard (baker /= 5)
    record 2

    -- Cooper does not live on the bottom floor
    guard (cooper /= 1)
    record 3

    -- Fletcher does not live on either the top or the bottom floor
    guard (fletcher /= 1 && fletcher /= 5)
    record 4

    -- Miller lives on a higher floor than does Cooper
    guard (miller > cooper)
    record 5

    -- Smith does not live on a floor adjacent to Fletcher's
    guard (abs (smith - fletcher) > 1)
    record 6

    -- Fletcher does not live on a floor adjacent to Cooper's
    guard (abs (fletcher - cooper) > 1)
    record 7

    return [ ("Baker", baker)
           , ("Cooper", cooper)
           , ("Fletcher", fletcher)
           , ("Miller", miller)
           , ("Smith", smith)
           ]

puzzleSolutions2 :: ListT (Writer [Int]) [(String, Int)]
puzzleSolutions2 = do
    -- I can not guarantee that this is the optimal solution,
    -- but this is absolutely better.
    -- The idea is to put constraints immediately after
    -- a new variable is introducted.
    -- To further narrow down the search space, variable
    -- with the strictest constraint comes first.
    -- In this example, I think the strictest restriction is put on Fletcher's floor,
    -- so I let it go first.
    -- Note that because the variables are introduced at different time, it's
    -- impossible to put "distinct" constraint first (however it's possible
    -- to put partial "distinct" constraint on current known varibles,
    -- but I choose not to do this for simplicity.)

    let floors = ListT (return [1..5])
    let record x = lift $ tell [x]

    fletcher <- floors
    -- Fletcher does not live on either the top or the bottom floor
    guard (fletcher /= 1 && fletcher /= 5)
    record 1

    smith    <- floors
    -- Smith does not live on a floor adjacent to Fletcher's
    guard (abs (smith - fletcher) > 1)
    record 2

    cooper   <- floors
    -- Cooper does not live on the bottom floor
    guard (cooper /= 1)
    record 3

    -- Fletcher does not live on a floor adjacent to Cooper's
    guard (abs (fletcher - cooper) > 1)
    record 4

    miller   <- floors
    -- Miller lives on a higher floor than does Cooper
    guard (miller > cooper)
    record 5

    baker    <- floors
    -- Baker does not live on the top floor
    guard (baker /= 5)
    record 6

    -- live on different floors
    guard (distinct [baker, cooper, fletcher, miller, smith])
    record 7

    return [ ("Baker", baker)
           , ("Cooper", cooper)
           , ("Fletcher", fletcher)
           , ("Miller", miller)
           , ("Smith", smith)
           ]

freqCount :: Ord a => [a] -> [(a,Int)]
freqCount xs = map (head &&& length) $ group $ sort xs

main :: IO ()
main = demoCondOrder

demoCondOrder :: IO ()
demoCondOrder = do
    let (answers1, records1) = runWriter $ runListT puzzleSolutions1
        (answers2, records2) = runWriter $ runListT puzzleSolutions2

    print answers1
    print answers2

    print $ freqCount records1
    -- [(1,120),(2,96),(3,78),(4,42),(5,15),(6,8),(7,1)]
    print $ freqCount records2
    --  [(1,3),(2,6),(3,24),(4,8),(5,8),(6,32),(7,1)]
