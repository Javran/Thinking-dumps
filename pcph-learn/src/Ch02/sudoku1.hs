module Main where
import Sudoku
import System.Environment
import Data.Maybe
import Control.DeepSeq
import Control.Parallel.Strategies hiding (parMap)
import Data.Functor

parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
   b <- rpar (f a)
   bs <- parMap f as
   return (b:bs)

main :: IO ()
main = do
  [f] <- getArgs
  file <- readFile f

  let puzzles = lines file
      -- solutions1: no parallelism is applied, the whole program
      -- runs sequentially
      solutions1 = map solve puzzles
      -- solutions2: statically partition tasks into half
      solutions2 =
          let (as,bs) = splitAt (length puzzles `div` 2) puzzles
          in runEval $ do
              -- put things into parallelism (force traversing the structure)
              as' <- rpar (force (map solve as))
              bs' <- rpar (force (map solve bs))
              -- wait for both results
              void $ rseq as'
              void $ rseq bs'
              return (as' ++ bs')
      solutions3 = runEval (parMap solve puzzles)

  print (length (filter isJust solutions3))
