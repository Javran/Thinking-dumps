module SP where

-- Swierstra and Duponcheel's Parsing Library

{-
  StaticParser contains:
  (1) a flag which indicates whether the parser accepts empty input
  (2) a list of tokens it might accept as the first token

  DynamicParser does the actual parsing (assuming the info
  in StaticParser is used to decide whether the input can be passed
  to this function for parsing)
-}

data StaticParser s = SP Bool [s]

newtype DynamicParser s a = DP ([s] -> (a,[s]))

data Parser s a = P (StaticParser s) (DynamicParser s a)
