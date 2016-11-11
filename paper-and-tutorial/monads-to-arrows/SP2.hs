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

    note that the original type is "([s] -> (a,[s])",
    we can still do "(((),[s]) -> (b,[s]))" to have "Parser s () b",
    I think I'm on the right track to say that "parse-time independency" is
    not addressed by this type change, but to fit this into the framework
    of Arrows, we need a Parser that takes both an input type and an output type.

  - why (SP True []) becomes the default static info (and && for composing)?

    about "SP True []", it actually makes sense. it's a Parser that accepts
    empty input and nothing more. "SP False []" will be one that accepts nothing
    (so having it in the chain of arrows will surely cause failure)

    it is confusing that we have multiple ways of composing parsers
    (TODO: with this in mind, "composeParser" might be a bad name to use)
    the trick is to look at the type signature:

    for the Category instance implementation:

      Parser s b c -> Parser s a b -> Parser s a c

    what we want to do is to run parsers in sequence, from a to b and end up
    with c, so every parser has to succeed.

    but for the instance of ArrowPlus:

      Parser s a b -> Parser s a b -> Parser s a b

    what we are doing is to create a parser by using two arguments,
    when the first parser fails, the second one is attempted.
    so we are doing a logical "or" operation on parsers.
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

instance Eq s => ArrowZero (Parser s) where
    -- the static info says this parser does not accept empty input
    -- and don't have an accepting list. so it always fails.
    -- the dynamic parser will never be called, so it's
    -- safe to assign it any value (actually the dynamic parser has to be of
    -- type (b,[s]) -> (c,[s]), we know that the only residence of "forall b c. b -> c"
    -- is just "undefined", and if we want some arrow whose input and output is
    -- universally quantified, we might just do something similar as well.
    zeroArrow = P (SP False []) (DP (error "dyn parser of zeroArrow is called"))

instance Eq s => ArrowPlus (Parser s) where
    (P (SP empty1 starters1) (DP p1)) <+>
      (P (SP empty2 starters2) (DP p2)) =
        P (SP (empty1 || empty2)
           (starters1 `union` starters2))
          (DP (\arg@(_,xs) -> case xs of
                   [] ->
                       (if empty1
                        then p1
                        else
                          if empty2
                          then p2
                          else
                            error "cannot accept empty input")
                       arg
                   (x:_) ->
                       (if x `elem` starters1
                        then p1
                        else
                          if x `elem` starters2
                          then p2
                          else (if empty1 then p1 else p2)) arg))

liftA2 :: Arrow a => (b -> c -> d) -> a e b -> a e c -> a e d
liftA2 op f g = (f &&& g) >>> arr (uncurry op)

(<**>) :: Eq s => Parser s a (b -> c) -> Parser s a b -> Parser s a c
(<**>) = liftA2 ($)
