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

{-
  a example of parsing "s"
  - static info: cannot accept empty input,
    and the only accepting leading token is just "s"
  - dynamic parser: does the actual parsing,
    the function looks partial but there is no risk
    as long as whoever executes this DynamicParser
    respects the static info before passing the input
    to it.
-}
symbol :: s -> Parser s s
symbol s = P (SP False [s]) (DP (\(_:xs) -> (s,xs)))

combineParser :: Eq a => Parser a b -> Parser a b -> Parser a b
combineParser
  (P (SP empty1 starters1) (DP p1))
  (P (SP empty2 starters2) (DP p2))
    = P (SP (empty1 || empty2) (starters1 ++ starters2))
        (DP (\xs -> case xs of
                 [] ->
                     -- left biased on tie
                     (if empty1 then p1 else p2) []
                 (x:_) ->
                     (if x `elem` starters1
                        then p1
                        else if x `elem` starters2
                               then p2
                               else
                                 -- TODO: a bit weird here, what to do on failure?
                                 -- TODO: do we have the assumption that empty1 || empty2 == True?
                                 if empty1 then p1 else p2) xs))
