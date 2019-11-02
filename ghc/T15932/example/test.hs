module Main where

import Control.DeepSeq
import Cov

main :: IO ()
main = do
  let f = fmap (+3) $ F (1, 2::Int)
  ((2 :: Int) <$ F (1, 2::Int)) `seq` putStrLn "Hi"
  f `deepseq` case f of
    F (1, 5) -> print "F Success"
    _ -> print "F Failure"
  let g = fmap (+3) $ G (1, 2::Int)
  g `deepseq` case g of
    G (1, 5) -> print "G Success"
    _ -> print "G Failure"
