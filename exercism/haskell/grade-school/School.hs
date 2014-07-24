module School
    ( School
    , empty
    , add
    , sorted
    , grade
    )
where

import Control.Arrow
import Data.Foldable (fold)
import Data.List

import qualified Data.Map.Strict as M

type School = M.Map Int [String]

-- | return an empty 'School'
empty :: School
empty = M.empty

-- | add new record to a 'School'
add :: Int -> String -> School -> School
add g n = M.insertWith (++) g [n]

-- | convert `School` to list, with elements sorted by grade and name
sorted :: School -> [(Int, [String])]
         -- convert to list, for each value, sort by name
sorted = (map . second) sort . M.toAscList

-- | sorted list of all names in a grade
grade :: Int -> School -> [String]
grade g =   M.lookup g
        >>> fold       -- Maybe [a] -> [a]
        >>> sort
