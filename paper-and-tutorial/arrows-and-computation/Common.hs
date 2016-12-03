module Common where

trace :: ((a,s) -> (b,s)) -> a -> b
trace f a = let (b,s) = f (a,s) in b

assoc :: ((a,b),c) -> (a,(b,c))
assoc ~(~(a,b),c) = (a,(b,c))

unassoc :: (a,(b,c)) -> ((a,b),c)
unassoc ~(a,~(b,c)) = ((a,b),c)
