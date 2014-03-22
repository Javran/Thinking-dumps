import Control.Monad (guard)
import Data.List

eightQueens :: [Int] -> [[Int]]
eightQueens b = do
    -- `b` builds up rows
    let len = length b
    if len == 8
       -- one solution available
       then return b
       else do
               -- `all1` applies column constriant
           let all1 = [0..7] \\ b
               -- `limitPairs` stores the place that we want to avoid.
               -- each pair is: {pos, offset}
               limitPairs = zip (reverse b) [len,len-1..]
               all2 = foldl
                          -- in this row, attackable cells are: [pos +/- offset]
                          (\curAll (pos,offset) -> curAll \\ [pos+offset, pos-offset] )
                          all1
                          -- apply all constraints
                          limitPairs
           next <- all2
           eightQueens (next:b)

solutionPrint :: [Int] -> IO ()
solutionPrint b = do
    putStr "  " -- header
    putStrLn $ concatMap show [(0::Int)..7]
    -- for each row (the solution place them in reversed order)
    mapM_ (\(c,pos) ->
               putStrLn (show c ++ " " ++ replicate pos ' ' ++ "x" ) )
              $ zip [(0::Int)..] (reverse b)
    putStrLn ""

main :: IO ()
main = mapM_ solutionPrint (eightQueens [])
