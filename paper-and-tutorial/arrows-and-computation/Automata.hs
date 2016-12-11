module Automata where

import qualified Control.Category as Cat
import Control.Arrow

-- an automaton accepts an input, gives an output
-- and changes its own state (returns a changed instance of itself)
-- note that the output also contains "Auto i o",
-- this could cause some confusion if one is not careful and returns
-- a wrong automaton as output.
newtype Auto i o = Auto (i -> (o, Auto i o))

arrAuto :: (i -> o) -> Auto i o
arrAuto f = auto
  where
    auto = Auto (\i -> (f i, auto))

-- composing two automata, this is achieved by threading the input
-- through two argument automata.
-- however, one needs to be really careful about what should be the output
-- automata: both "composeAuto f g" and "composeAuto f' g'" fits
-- but only the latter is the correct implementation.
-- so here the type is not sophisticated enough to prevent this kind of error.
composeAuto :: Auto a b -> Auto b c -> Auto a c
composeAuto (Auto f) (Auto g) =
    Auto (\i -> let (b,f') = f i
                    (c,g') = g b
                in (c,f' `composeAuto` g'))

instance Cat.Category Auto where
    id = arrAuto id
    g . f = f `composeAuto` g

instance Arrow Auto where
    arr = arrAuto
    first (Auto f) =
        Auto (\(a,b) ->
              let (c,f') = f a in ((c,b), first f'))

instance ArrowLoop Auto where
    loop (Auto f) = Auto $ \ b ->
        -- basically, we need to apply something to "f" and keep going,
        -- so try "f (b,undefined)" first and see the resulting type,
        -- from which we can replace "undefined" with the proper thing from function's
        -- output.
        -- TODO: still, I have no idea how to make sense of this,
        -- but this strategy works for me.
        let (~(c,d), f') = f (b,d) in (c, loop f')

instance ArrowChoice Auto where
    left (Auto f) = ar
      where
        ar = Auto $ \ebd -> case ebd of
                Left b ->
                    let (c,a') = f b
                    in (Left c, left a')
                Right d -> (Right d, ar)

-- TODO: why ArrowCircuit must be ArrowLoop while
-- we are not actually using this fact in the implementation?
class ArrowLoop a => ArrowCircuit a where
    delay :: b -> a b b

instance ArrowCircuit Auto where
    delay b = Auto $ \ b' -> (b, delay b')
