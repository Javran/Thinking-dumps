module RunLength
  ( decode
  , encode
  )
where

import Data.Char
import Data.List

decode :: String -> String
decode [] = []
decode xs = case ds of
  [] -> y : decode ys
  _ : _ -> replicate (read ds) y <> decode ys
  where
    -- this pattern match will fail on an ill-formed encoded string,
    -- in which a number is not immediately following by anything.
    (ds, y : ys) = span isDigit xs

encode :: String -> String
encode =
  concatMap
    (\xs ->
       let l = length xs
        in if l > 1
             then show l <> take 1 xs
             else xs)
    . group
