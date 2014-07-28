module Problem38 where

import System.CPUTime
import Text.Printf

import Problem34 (totient)
import Problem37 (phi)

-- let's just take time as a measure

main :: IO ()
main = do
    t1 <- getCPUTime
    print $ totient 10090
    t2 <- getCPUTime
    print $ phi 10090
    t3 <- getCPUTime
    printf "diff1=%d, diff2=%d\n" (t2-t1) (t3-t2)
