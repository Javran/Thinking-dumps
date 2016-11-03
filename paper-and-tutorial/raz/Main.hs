module Main where

import Raz

main :: IO ()
main = do
    let Just mRaz = fromNonEmptyList [1..20 :: Int]
    raz <- mRaz
    print (toList (unfocus raz))
    pure ()
