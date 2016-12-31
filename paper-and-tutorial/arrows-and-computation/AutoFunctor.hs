module AutoFunctor where

import qualified Control.Category as Cat
import Control.Arrow
import Data.Function

newtype AutoFunctor a i o = AF (a i (o, AutoFunctor a i o))

arrAF :: Arrow a => (i -> o) -> AutoFunctor a i o
arrAF f = fix $ \arrow -> AF (arr (\i -> (f i,arrow)))

compAF :: Arrow ar => AutoFunctor ar a b -> AutoFunctor ar b c -> AutoFunctor ar a c
compAF (AF f) (AF g) =
    AF (f >>> first g >>> arr assoc >>> second (arr (\(g',f') -> compAF f' g')))
  where
    assoc ((a,b),c) = (a,(b,c))
