module Problem41 where

import Control.Arrow

import Problem22 (range)
import Problem40 (goldbach)

goldbachList :: Int -> Int -> [(Int,Int)]
goldbachList l r = map goldbach
                 . filter (\x -> x > 2 && even x)
                 $ range l r

goldbachList' :: Int -> Int -> Int -> [(Int, Int)]
                      -- x and y are both greater than n
goldbachList' l r n = filter (uncurry (&&) . ((> n) *** (> n)))
                    $ goldbachList l r

main :: IO ()
main = do
    print $ goldbachList 9 20
    print $ goldbachList' 4 2000 50
