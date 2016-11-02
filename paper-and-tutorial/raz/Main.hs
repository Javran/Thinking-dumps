module Main where

import Control.Monad
import Control.Monad.Random

import Raz

main :: IO ()
main = do
    let r1 = singleton (10 :: Int)
    rFinal <- foldM (\acc i -> insert L i acc) r1 [1..9]
    print (toList (unfocus rFinal))
    pure ()
