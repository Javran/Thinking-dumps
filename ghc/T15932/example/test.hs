{-# OPTIONS_GHC -ddump-to-file -ddump-ds -fhpc #-}
module Main where

import Cov

main :: IO ()
main = do
  let f@(F(f0,f1)) = fmap (+3) $ F (1, 2::Int)
  -- ((2 :: Int) <$ F (1, 2::Int)) `seq` putStrLn "Hi"
  f0 `seq` f1 `seq` case f of
    F (1, 5) -> print "F Success"
    _ -> print "F Failure"
  let g@(G (g0,g1)) = fmap (+3) $ G (1, 2::Int)
  g0 `seq` g1 `seq` case g of
    G (1, 5) -> print "G Success"
    _ -> print "G Failure"
