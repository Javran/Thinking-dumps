module TwelveDays
  ( recite
  )
where

import Data.List

recite :: Int -> Int -> [String]
recite start stop = take (stop - start + 1) $ drop (start -1) fullContents

fullContents, firstParts, remainingSentences, listing :: [String]
fullContents = zipWith (<>) firstParts remainingSentences
firstParts =
  [ "On the " <> nth <> " day of Christmas my true love gave to me: "
  | nth <-
      words
        "first second third fourth fifth sixth \
        \seventh eighth ninth tenth eleventh twelfth"
  ]
remainingSentences = drop 4 a : as
  where
    (a : as) =
      fmap (intercalate ", ") . reverse . init $ tails listing
listing =
  [ "twelve Drummers Drumming"
  , "eleven Pipers Piping"
  , "ten Lords-a-Leaping"
  , "nine Ladies Dancing"
  , "eight Maids-a-Milking"
  , "seven Swans-a-Swimming"
  , "six Geese-a-Laying"
  , "five Gold Rings"
  , "four Calling Birds"
  , "three French Hens"
  , "two Turtle Doves"
  , "and a Partridge in a Pear Tree."
  ]
