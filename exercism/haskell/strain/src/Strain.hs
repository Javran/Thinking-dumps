module Strain
    ( keep
    , discard
    )
where

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p (x:xs) = let rest = keep p xs
                in if p x then x:rest else rest

discard :: (a -> Bool) -> [a] -> [a]
discard p = keep (not . p)
