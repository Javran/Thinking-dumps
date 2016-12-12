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

-- there must be a way to turn instance of ArrowApply into ArrowChoice.
-- if so, all we need is to try implementing the following function.
-- TODO: impl done, let's get refactor going next time.
arrAppToChoice :: ArrowApply arrow
               => arrow i o
               -> arrow (Either i a) (Either o a)
arrAppToChoice arrow = arrFanin (arrow >>> arr Left) (arr Right)
  where
    arrPlus f g = arrAppToChoice f >>> arr mirror >>> arrAppToChoice g >>> arr mirror
      where
        mirror (Left x) = Right x
        mirror (Right x) = Left x
    arrFanin f g = arrPlus f g >>> arr untag
      where
        untag (Left x) = x
        untag (Right x) = x
