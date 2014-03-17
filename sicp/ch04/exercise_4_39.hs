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

import Control.Monad (guard)
import Data.List

import Control.Monad.List
import Control.Monad.Writer
import Control.Arrow

demoSearchSpaceEffect :: IO ()
demoSearchSpaceEffect = do
    let space1D :: [Int]
        space1D = [1..100]
        -- the seach space seems to be the same,
        -- but this is just because the laziness:
        -- unless we need the next value (`try-again`),
        -- we don't have to try everything in the potential search space.
        -- the facts are:
        -- * only valid solutions can pass all the guards
        -- * if a guard has the strongest constraint than others,
        --   this guard should be used as early as possible.
        --   The reason is that this guard will narrow down more search space
        --   than any other guards, and this narrowing down will prevent those
        --   "already invalid" solutions from further computations.
        spaceAll = do
            i <- space1D
            j <- space1D
            k <- space1D
            return (i,j,k)

        pythagorean11 = do
            (i,j,k) <- spaceAll
            guard $ i <= j && j <= k
            return (i,j,k)
        pythagorean12 = do
            (i,j,k) <- pythagorean11
            guard $ i * i + j * j == k * k
            return (i,j,k)

        pythagorean21 = do
            (i,j,k) <- spaceAll
            guard $ i * i + j * j == k * k
            return (i,j,k)
        pythagorean22 = do
            (i,j,k) <- pythagorean21
            guard $ i <= j && j <= k
            return (i,j,k)

    putStrLn "i <= j <= k first, then i * i + j * j == k * k:"
    print $ length pythagorean11
    print $ length pythagorean12

    putStrLn "i * i + j * j == k * k first, then i <= j <= k:"
    print $ length pythagorean21
    print $ length pythagorean22

    putStrLn "results are the same?"
    print $ pythagorean12 == pythagorean22

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
    let floors = ListT (return [1..5])
    let record x = lift $ tell [x]

    baker    <- floors
    cooper   <- floors
    fletcher <- floors
    miller   <- floors
    smith    <- floors

    -- Smith does not live on a floor adjacent to Fletcher's
    guard (abs (smith - fletcher) > 1)
    record 1

    -- Fletcher does not live on a floor adjacent to Cooper's
    guard (abs (fletcher - cooper) > 1)
    record 2

    -- live on different floors
    guard (distinct [baker, cooper, fletcher, miller, smith])
    record 3

    -- Baker does not live on the top floor
    guard (baker /= 5)
    record 4

    -- Cooper does not live on the bottom floor
    guard (cooper /= 1)
    record 5

    -- Fletcher does not live on either the top or the bottom floor
    guard (fletcher /= 1 && fletcher /= 5)
    record 6

    -- Miller lives on a higher floor than does Cooper
    guard (miller > cooper)
    record 7

    return [ ("Baker", baker)
           , ("Cooper", cooper)
           , ("Fletcher", fletcher)
           , ("Miller", miller)
           , ("Smith", smith)
           ]

puzzleSolutions3 :: ListT (Writer [Int]) [(String, Int)]
puzzleSolutions3 = do
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

freqCount :: Ord a => [a] -> [(a,Int)]
freqCount xs = map (head &&& length) $ group $ sort xs

main :: IO ()
main = do
    demoSearchSpaceEffect
    demoCondOrder

demoCondOrder :: IO ()
demoCondOrder = do
    -- the only difference is that the order of constraints are changed
    let (answers1, records1) = runWriter $ runListT puzzleSolutions1
        (answers2, records2) = runWriter $ runListT puzzleSolutions2
        (answers3, records3) = runWriter $ runListT puzzleSolutions3

    -- we observe how the search space decreases by logging whenever
    -- a guard is passed.
    print answers1
    print answers2
    print answers3

    -- then we count frequence to see after each guard, how many possibilities
    -- are remained.
    print $ freqCount records1
    -- output: [(1,120),(2,96),(3,78),(4,42),(5,15),(6,8),(7,1)]
    print $ freqCount records2
    -- output: [(1,1500),(2,750),(3,36),(4,32),(5,25),(6,7),(7,1)]
    print $ freqCount records3

    -- we can see that the first one is better
