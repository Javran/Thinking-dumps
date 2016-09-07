module TypedFinal3 where

-- notice that we no longer keep environment "h" in the definition
-- and Haskell will be taking over binders
class Symantics repr where
    int :: Int -> repr Int
    add :: repr Int -> repr Int -> repr Int
    lam :: (repr a -> repr b) -> repr (a -> b)
    app :: repr (a -> b) -> repr a -> repr b

th1 :: Symantics repr => repr Int
th1 = add (int 1) (int 2)

th2 :: Symantics repr => repr (Int -> Int)
th2 = lam (\x -> add x x)

th3 :: Symantics repr => repr ((Int -> Int) -> Int)
th3 = lam (\x -> add (app x (int 1)) (int 2))

newtype R a = R { unR :: a }

instance Symantics R where
    int = R
    add e1 e2 = R (unR e1 + unR e2)

    lam f = R (unR . f . R)
    app e1 e2 = R $ unR e1 (unR e2)

eval :: R a -> a
eval = unR

type VarCounter = Int
newtype S a = S { unS :: VarCounter -> String }

instance Symantics S where
    int x = S $ \_ -> show x
    add e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ "+" ++ unS e2 h ++ ")"

    lam e = S $ \h ->
        let x = "x" ++ show h
        in "(\\" ++ x  ++ " -> " ++
           unS (e (S $ const x {- TODO: I'm not sure of this part, need examples -}))
               (succ h) ++ ")"
    app e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S a -> String
view e = unS e 0
