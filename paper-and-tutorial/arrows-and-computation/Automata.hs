module Automata where

import qualified Control.Category as Cat
import Control.Arrow

-- an automaton accepts an input, gives an output
-- and changes its own state (returns a changed instance of itself)
newtype Auto i o = Auto (i -> (o, Auto i o))

arrAuto :: (i -> o) -> Auto i o
arrAuto f = auto
  where
    auto = Auto (\i -> (f i, auto))

composeAuto :: Auto a b -> Auto b c -> Auto a c
composeAuto (Auto f) (Auto g) =
    Auto (\i -> let (b,f') = f i
                    (c,g') = g b
                in (c,f' `composeAuto` g'))
