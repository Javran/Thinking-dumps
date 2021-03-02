module Raindrops
  ( convert
  )
where

convert :: Int -> String
convert x = if null result then show x else result
  where
    result =
      3 ?-> "Pling"
        <> 5 ?-> "Plang"
        <> 7 ?-> "Plong"
    b ?-> v =
      if x `rem` b == 0
        then v
        else ""
