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

    -- about "(f (S $ const x) _)" part,
    -- recall that "f" is *not* an expression but a function,
    -- in this final embedding, it expects an argument, in this case the argument
    -- is just the variable name we just assigned for pretty print (the value bound to "x")
    -- thus "S $ const x".
    lam f = S $ \h ->
        let x = "x" ++ show h
        in "(\\" ++ x  ++ " -> " ++
           unS (f (S $ const x))
               (succ h) ++ ")"
    app e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " " ++ unS e2 h ++ ")"

view :: S a -> String
view e = unS e 0

class MulSYM repr where
    mul :: repr Int -> repr Int -> repr Int

class BoolSYM repr where
    bool :: Bool -> repr Bool
    leq :: repr Int -> repr Int -> repr Bool
    if_ :: repr Bool -> repr a -> repr a -> repr a

class FixSYM repr where
    fix :: (repr a -> repr a) -> repr a

tpow :: (Symantics repr, MulSYM repr, FixSYM repr, BoolSYM repr) => repr (Int -> Int -> Int)
tpow = lam (\x -> fix (\self -> lam (\n ->
                                     if_
                                       (leq n (int 0))
                                       (int 1)
                                       (mul
                                          x
                                          (app self (add n (int (-1))))))))

tpow7 :: (Symantics repr, MulSYM repr, FixSYM repr, BoolSYM repr) => repr (Int -> Int)
tpow7 = lam (\x -> (tpow `app` x) `app` int 7)

tpow72 :: (Symantics repr, MulSYM repr, FixSYM repr, BoolSYM repr) => repr Int
tpow72 = tpow7 `app` int 2

instance MulSYM R where
    mul e1 e2 = R $ unR e1 * unR e2

instance BoolSYM R where
    bool = R
    leq e1 e2 = R $ unR e1 <= unR e2
    if_ be et ee = if unR be then et else ee

instance FixSYM R where
    fix f = R $ fx (unR . f . R)
      where
        -- just like Data.Function.fix
        fx f' = let x = f' x in x

-- no change to the evaluator, "eval tpow72" should work as expected

instance MulSYM S where
    mul e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " * " ++ unS e2 h ++ ")"

instance BoolSYM S where
    bool= S . const . show
    leq e1 e2 = S $ \h ->
        "(" ++ unS e1 h ++ " <= " ++ unS e2 h ++ ")"
    if_ be et ee = S $ \h ->
        "if " ++ unS be h
        ++ " then " ++ unS et h
        ++ " else " ++ unS ee h

instance FixSYM S where
    -- notice two things:
    -- 1. the similarity between this and "lam" case, in both case a new variable
    --    is introduced and the counter bumps in the same way.
    -- 2. "fix" case has no explicit recursion.
    fix e = S $ \h ->
        let self = "self" ++ show h
        in "(fix " ++ self ++ " . " ++
           unS (e (S $ const self)) (succ h) ++ ")"
