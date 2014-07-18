module Problem29 where

import Control.Arrow
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map.Strict as M

lSort :: [ [a] ] -> [ [a] ]
lSort = sortBy (compare `on` length)

lfSort :: [ [a] ] -> [ [a] ]
lfSort = concatMap snd -- remove freq info
       -- 'length . snd' should be the freq accessor
       . sortBy (compare `on` length . snd )
       . M.toList
       -- key: list length, value: a list of list of length key
       . M.fromListWith (++) -- index lists by their keys
       . map (length &&& (:[]))

main :: IO ()
-- the result isn't in exact order, but it meets all requirements in the problem
-- additionally, we have the benefit of not using the Ord typeclass
-- which is the most general one I can think of
main = print $ lfSort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
