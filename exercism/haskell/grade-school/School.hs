module School
    ( School
    , empty
    , add
    , sorted
    , grade
    )
where

import Control.Arrow
import Data.Function
import Data.List
import Data.Maybe

import qualified Data.Map.Strict as M

type School = M.Map Int [String]

-- | return an empty 'School'
empty :: School
empty = M.empty

-- | add new record to a 'School'
add :: Int -> String -> School -> School
add g n = M.alter (Just . maybe onFailure onSuccess) g
    where
        -- try searching by key and alter the value
        onSuccess = (n :) -- ^ found, prepend new name to it
        onFailure = [n]   -- ^ not found, make a singleton

-- | convert `School` to list, with elements sorted by grade and name
sorted :: School -> [(Int, [String])]
sorted =   M.toList                  -- convert to list
       >>> sortBy (compare `on` fst) -- sort by comparing grade
       >>> (map . second) sort       -- walk into the second part(name list)
                                     -- of each element, sort it.
-- | sorted list of all names in a grade
grade :: Int -> School -> [String]
grade g =   M.lookup g   -- lookup g from the dict
        >>> fromMaybe [] -- convert result to list (listToMaybe >>> concat)
        >>> sort         -- sort result
