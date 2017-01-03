module Exercise4 where

import Common
import Control.Arrow

-- the exercise itself is wrong. "foo" doesn't type check, at least "arr g" should
-- present to lift it from pure function to whatever the intended arrow is

{-
foo :: Arrow ar => ar a1 a2 -> (b1 -> b2) -> ar (a1,b1) (a2,b2)
foo f g = first f >>> (cross id g)
-}
