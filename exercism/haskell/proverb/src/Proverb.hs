module Proverb
  ( recite
  )
where

import Data.List

recite :: [String] -> String
recite [] = ""
recite xs@(hd : _) =
  intercalate "\n" $
    [ "For want of a " <> a <> " the " <> b <> " was lost."
    | (a, b) <- zip xs (tail xs)
    ]
      <> ["And all for the want of a " <> hd <> "."]
