module Common where

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
