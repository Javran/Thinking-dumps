module Count where

import qualified Control.Category as Cat
import Control.Arrow

{-
there is nothing prevent us from having a more abstract type instead
of using Int, but that complicates many type signatures and doesn't
do us much good.
-}
data Count a i o = Count Int (a i o)

arrCount :: Arrow a => (i -> o) -> Count a i o
arrCount f = Count 0 (arr f)

compCount :: Cat.Category ar => Count ar a b -> Count ar b c -> Count ar a c
compCount (Count a f) (Count b g) = Count (a+b) (f >>> g)

instance Arrow arr => Cat.Category (Count arr) where
    id = arrCount id
    g . f = compCount f g

firstCount :: Arrow ar => Count ar a b -> Count ar (a,d) (b,d)
firstCount (Count a f) = Count a (first f)

instance Arrow ar => Arrow (Count ar) where
    arr = arrCount
    first = firstCount

instance ArrowChoice ar => ArrowChoice (Count ar) where
    left (Count a f) = Count a (left f)
