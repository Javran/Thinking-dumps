module Common where

import Control.Arrow

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

-- TODO: I think there must be a way to turn instance of ArrowApply into ArrowChoice.
-- if so, all we need is to try implementing the following function.
-- (for now I'm not sure how to do so though: probably the following stuff
-- is entirely wrong because we have to do it in a somehow point-free manner)
arrAppToChoice :: ArrowApply arrow => arrow i o -> arrow (Either i a) (Either o a)
arrAppToChoice arrow = arr $ \ e -> case e of
    Left i -> let x = returnA (arrow,i) in _
    Right a -> Right a
