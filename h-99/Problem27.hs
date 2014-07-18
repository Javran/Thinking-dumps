module Problem27 where

import Control.Arrow

-- pick up a set of size n
pickSet :: Int -> [a] -> [([a],[a])]
pickSet 0 xs = [([],xs)]
pickSet _ [] = []
pickSet n (x:xs) = map (first  (x:)) (pickSet (n-1) xs)
                ++ map (second (x:)) (pickSet n     xs)

group :: [Int] -> [a] -> [ [[a]] ]
group [] _ = [ [] ]
group (n:ns) xs = do (as,bs) <- pickSet n xs
                     cs <- group ns bs
                     return (as:cs)

group3 :: [a] -> [ [[a]] ]
group3 = group [2,3,4]

main :: IO ()
main = do
    let xs = ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
    print $ length $ group3 xs
    print $ length $ group [2,2,5] xs
