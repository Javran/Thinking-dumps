module FoodChain
  ( song
  ) where

import Data.List

song :: String
song = ""

endings :: [String]
endings =
    [ "She swallowed the cow to catch the goat."
    , "She swallowed the goat to catch the dog."
    , "She swallowed the dog to catch the cat."
    , "She swallowed the cat to catch the bird."
    , "She swallowed the bird to catch the spider that wriggled and jiggled and tickled inside her."
    , "She swallowed the spider to catch the fly."
    , "I don't know why she swallowed the fly. Perhaps she'll die."
    ]

initParas :: [[String]]
initParas = zipWith (:) firstLines
    [ []
    , [ "It wriggled and jiggled and tickled inside her." ]
    , [ "How absurd to swallow a bird!" ]
    , [ "Imagine that, to swallow a cat!" ]
    , [ "What a hog, to swallow a dog!" ]
    , [ "Just opened her throat and swallowed a goat!" ]
    , [ "I don't know how she swallowed a cow!" ]
    ]
  where
    firstLines = map swallow (words "fly spider bird cat dog goat cow")
      where
        swallow x = "I know an old lady who swallowed a " ++ x ++ "."

lastPara :: [String]
lastPara =
    [ "I know an old lady who swallowed a horse."
    , "She's dead, of course!"
    ]

paragraphs :: [ [String] ]
paragraphs = initParas ++ [lastPara]
