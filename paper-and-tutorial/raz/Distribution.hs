{-# LANGUAGE
    ScopedTypeVariables
  , TypeFamilies
  , TupleSections
  #-}
module Distribution where

-- for generating "result.csv" to visualize how the distribution of rndLevel looks like

import Raz (rndLevel)
import qualified Data.Vector.Unboxed.Mutable as VUM
import Control.Monad
import System.Environment

runExperiment :: Int -> IO [(Int,Int)]
runExperiment n = do
    vec <- VUM.unsafeNew 32 -- 0..31
    VUM.set vec 0
    (rnds :: [Int]) <- replicateM n rndLevel
    mapM_ (VUM.modify vec (succ :: Int -> Int)) rnds
    mapM (\ind -> (ind,) <$> VUM.read vec ind) [0..31]

main :: IO ()
main = do
    as <- getArgs
    let n = case as of
              [nRaw] | [(n',"")] <- reads nRaw -> n'
              _ -> 1000
    runExperiment n >>= mapM_ (\(x,y) -> putStrLn $ show x ++ "," ++ show y)
