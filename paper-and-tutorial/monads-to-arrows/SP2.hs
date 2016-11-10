module SP2 where

import Control.Arrow
import Data.List
import qualified Control.Category as Cat

-- TODO: not sure how does "b" affect things, but let's go ahead and try it out.
data StaticParser s = SP Bool [s]

newtype DynamicParser s a b = DP ((a,[s]) -> (b,[s]))

data Parser s a b = P (StaticParser s) (DynamicParser s a b)

{-
  - why the parsing function is of type ((a,[s]) -> (b,[s])) ?
    I feel the paper is not really doing a good job explaining
    the change of the type signature. what I know is that, it now becomes
    a function, that consumes "[s]", and turns "a" into "b" accordingly.
    but why this captures the idea of "static properties of a parser should not
    depend on parse-time input" is not explained at all.
  - why (SP True []) becomes the default static info (and && for composing)?
-}

composeParser :: Eq s => Parser s a b -> Parser s b c -> Parser s a c
composeParser
  (P (SP empty1 starters1) (DP p1))
  (P (SP empty2 starters2) (DP p2)) =
        P (SP (empty1 && empty2)
              (starters1 `union` if empty1 then starters2 else []))
          (DP (p2 . p1))

instance Eq s => Cat.Category (Parser s) where
    -- because of we are implementing (.) composition,
    -- the pattern matching has to be written backwards
    (.) = flip composeParser
    id = P (SP True []) (DP id)

instance Eq s => Arrow (Parser s) where
    arr f = P (SP True []) (DP (first f))
    first (P sp (DP p)) =
        P sp (DP (\((b,d),s) -> let (c,s') = p (b,s) in ((c,d),s')))
