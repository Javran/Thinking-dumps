module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ z [] = z
foldl' f z (x:xs) = let z' = f z x
                    in z' `seq` foldl' f (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ seed [] = seed
foldr f seed (x:xs) = f x (foldr f seed xs)

length :: [a] -> Int
length = foldl' (\acc _ -> acc + 1) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\i acc -> f i :acc) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\i acc -> if p i then i:acc else acc) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
