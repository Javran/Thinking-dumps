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

{-
  "Parser a" cannot be an instance of Monad
  because the implementation of ">>=" will be problematic:
  the type of this particular ">>=" will be:
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  note that we are combining two Parsers together, which requires
  static info from both Parsers, but we can only get the static info
  (from the second Parser) by apply something of type "a" to it,
  at which point the first parser should have been executed.
  this suggests the static info of the second Parser depends
  on the parsing result of the first one, which is not the case.
  (if we know the second function to "(>>=)" returns a Parser without
  using its input "a", then it's safe to "cheat" a bit by feeding it "undefined",
  since the value will never be used anyway. But in reality we have to face
  an obscure function, so this strategy will not work.)
-}
