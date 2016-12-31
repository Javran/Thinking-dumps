module AutoFunctor where

import qualified Control.Category as Cat
import Control.Arrow
import Data.Function

newtype AutoFunctor a i o = AF (a i (o, AutoFunctor a i o))

arrAF :: Arrow a => (i -> o) -> AutoFunctor a i o
arrAF f = fix $ \arrow -> AF (arr (\i -> (f i,arrow)))

-- note that argument order are changed to allow us using "uncurry" to same some work
compAF :: Arrow ar => AutoFunctor ar b c -> AutoFunctor ar a b -> AutoFunctor ar a c
compAF (AF g) (AF f) =
    AF (f >>> first g >>> arr assoc >>> second (arr (uncurry compAF)))
  where
    assoc ((a,b),c) = (a,(b,c))

instance Arrow ar => Cat.Category (AutoFunctor ar) where
    id = arrAF id
    (.) = compAF
