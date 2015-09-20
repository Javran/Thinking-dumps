module Raindrops
  ( convert
  ) where

import Data.Maybe
import Data.Monoid ()

convert :: Int -> String
convert x =
    -- whether we should output the result or just show the number in string form
    -- depends on the intermediate result
    if null result
      then show x
      else result
  where
    -- apply the function to all 3 testers for an intermediate result
    result = mconcat (mapMaybe ($ x) [pling, plang, plong])
    -- if @n@ contains factor "f" then "v" is included in the result
    test f v n =
        if n `rem` f == 0
           then Just v
           else Nothing
    -- testers for different factors & outputs
    pling = test 3 "Pling"
    plang = test 5 "Plang"
    plong = test 7 "Plong"
