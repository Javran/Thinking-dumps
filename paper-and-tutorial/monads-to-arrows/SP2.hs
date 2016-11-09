module SP2 where

import Control.Arrow
import Data.List
import qualified Control.Category as Cat

-- TODO: not sure how does "b" affect things, but let's go ahead and try it out.
data StaticParser s = SP Bool [s]

newtype DynamicParser s a b = DP ((a,[s]) -> (b,[s]))

data Parser s a b = P (StaticParser s) (DynamicParser s a b)

instance Eq s => Cat.Category (Parser s) where
    -- because of we are implementing (.) composition,
    -- the pattern matching has to be written backwards
    P (SP empty2 starters2) (DP p2) .
      P (SP empty1 starters1) (DP p1) =
        P (SP (empty1 && empty2)
              (starters1 `union` if empty1 then starters2 else []))
          (DP (p2 . p1))

instance Eq s => Arrow (Parser s) where
    arr f = P (SP True []) (DP (first f))
    first (P sp (DP p)) =
        P sp (DP (\((b,d),s) -> let (c,s') = p (b,s) in ((c,d),s')))
