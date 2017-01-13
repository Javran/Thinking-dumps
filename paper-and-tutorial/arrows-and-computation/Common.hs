module Common where

import Control.Arrow

mkPair :: Arrow ar => b -> ar c (b,c)
mkPair b = arr (\c -> (b,c))

trace :: ((a,s) -> (b,s)) -> a -> b
trace f a = let (b,s) = f (a,s) in b

assoc :: ((a,b),c) -> (a,(b,c))
assoc ~(~(a,b),c) = (a,(b,c))

unassoc :: (a,(b,c)) -> ((a,b),c)
unassoc ~(a,~(b,c)) = ((a,b),c)

distr :: (Either a b, c) -> Either (a,c) (b,c)
distr ~(e,c) = case e of
    Left a -> Left (a,c)
    Right b -> Right (b,c)

mirror :: Either a b -> Either b a
mirror (Left x) = Right x
mirror (Right y) = Left y

-- the following function is a proof that every instance of ArrowApply can be turned
-- into an instance of ArrowChoice by using this function as "left"
-- note that ArrowApply is just Monad in disguise,
-- actually the implementation is very similar to that of Kleisli arrows.
arrAppLeft :: ArrowApply arrow
           => arrow i o
           -> arrow (Either i a) (Either o a)
arrAppLeft arrow = arrFanin (arrow >>> arr Left) (arr Right)
  where
    arrPlus f g = arrAppLeft f >>> arr mirror >>> arrAppLeft g >>> arr mirror
    arrFanin f g = arrPlus f g >>> arr untag
      where
        untag = either id id

cross :: (a1 -> a2) -> (b1 -> b2) -> (a1,b1) -> (a2,b2)
cross f g ~(a,b) = (f a, g b)

-- I'm not entirely sure why ArrowCircuit has to imply ArrowLoop,
-- but this might be just for convenient concerns
class ArrowLoop a => ArrowCircuit a where
    delay :: b -> a b b
