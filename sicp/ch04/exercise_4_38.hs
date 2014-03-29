import Control.Monad (guard)
import Data.List (nub)

distinct :: Eq e => [e] -> Bool
distinct xs = length xs == length ys
    where
        ys = nub xs

puzzleSolutions :: [[(String, Int)]]
puzzleSolutions = do
    let floors = [1..5]

    baker    <- floors
    cooper   <- floors
    fletcher <- floors
    miller   <- floors
    smith    <- floors

    -- live on different floors
    guard (distinct [baker, cooper, fletcher, miller, smith])

    -- Baker does not live on the top floor
    guard (baker /= 5)

    -- Cooper does not live on the bottom floor
    guard (cooper /= 1)

    -- Fletcher does not live on either the top or the bottom floor
    guard (fletcher /= 1 && fletcher /= 5)

    -- Miller lives on a higher floor than does Cooper
    guard (miller > cooper)

    {- remove this condition according to the exercise
    -- Smith does not live on a floor adjacent to Fletcher's
    guard (abs (smith - fletcher) > 1)
    -}

    -- Fletcher does not live on a floor adjacent to Cooper's
    guard (abs (fletcher - cooper) > 1)

    return [ ("Baker", baker)
           , ("Cooper", cooper)
           , ("Fletcher", fletcher)
           , ("Miller", miller)
           , ("Smith", smith)
           ]

main :: IO ()
main = do
    putStrLn $ show (length puzzleSolutions) ++ " Solutions found."
    print puzzleSolutions

