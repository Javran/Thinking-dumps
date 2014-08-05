module Problem55 where

import Control.Applicative

import BinaryTree

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [singleton 'x']
cbalTree n = do
    (l,r) <- splitAlmostEqual (n-1)
    Branch 'x' <$> cbalTree l <*> cbalTree r

-- | split an integer into two parts (l,r)
--   where / abs(l-r) <= 1 /
splitAlmostEqual :: Int -> [(Int, Int)]
splitAlmostEqual n
    | even n = [(half,half)]
    | otherwise = [(half, half+1), (half+1,half)]
    where
        half = n `div` 2

main :: IO ()
main = mapM_ print $ cbalTree 4
