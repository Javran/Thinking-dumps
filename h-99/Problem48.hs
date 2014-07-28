module Problem48 where

import Control.Monad

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n f = mapM_ makeAndPrintLn  (replicateM n [True,False])
    where makeAndPrintLn xs = putStrLn . unwords . map show
                            $ xs ++ [f xs]

main :: IO ()
main = tablen 3 (\[a,b,c] -> a && (b || c) == a && b || a && c)
