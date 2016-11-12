module StreamProcessor where

{-
  a stream processor maps a stream of input messages
  into a stream of output messages, but is represented
  by an abstract data type.
-}
data SP a b
  = Put b (SP a b)
  | Get (a -> SP a b)

put :: b -> SP a b -> SP a b
put = Put

get :: (a -> SP a b) -> SP a b
get = Get

spArr :: (a -> b) -> SP a b
spArr f = sp'
  where
    -- get one value, apply "f", put it back, and repeat
    sp' = Get (\x -> Put (f x) sp')

-- TODO: looks weird, need explanation
spCompose :: SP a b -> SP b c -> SP a c
spCompose sp1 sp2 = case sp2 of
    Put c sp2' -> Put c (sp1 `spCompose` sp2')
    Get f2 -> case sp1 of
        Put b sp1' -> sp1' `spCompose` f2 b
        Get f1 -> Get (\a -> f1 a `spCompose` Get f2)
