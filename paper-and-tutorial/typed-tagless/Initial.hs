module Initial where

-- expressions are represented as datatypes
data Exp
  = Lit Int
  | Neg Exp
  | Add Exp Exp

ti1 :: Exp
ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

-- expression interpreted through a function
eval :: Exp -> Int
eval e = case e of
    Lit n -> n
    Neg e' -> - eval e'
    Add e1 e2 -> eval e1 + eval e2

view :: Exp -> String
view e = case e of
    Lit n -> show n
    Neg e' -> "(-" ++ view e' ++ "-)"
    Add e1 e2 -> "(" ++ view e1 ++ " + " ++ view e2 ++ ")"

til1 :: [Exp]
til1 = [Lit 1, Add (Lit 1) (Lit 3)]

-- the inital view of doing negation pushing down
-- is just to pattern match on expressions and deal with them accordingly.
-- in the example above GHC should be smart enough to figure out all possible cases
-- are covered.
pushNeg :: Exp -> Exp
pushNeg e = case e of
    Lit _ -> e
    Neg (Lit _) -> e
    Neg (Neg e') -> pushNeg e' -- double negation cancels out
    Neg (Add e1 e2) ->
        -- note that this part is not structural inductive
        Add (pushNeg (Neg e1)) (pushNeg (Neg e2))
    Add e1 e2 -> Add (pushNeg e1) (pushNeg e2)

flata :: Exp -> Exp
flata e = case e of
    Lit _ -> e
    Neg _ -> e
    Add (Add e1 e2) e3 -> flata (Add e1 (Add e2 e3))
    Add e1 e2 -> Add (flata e1) (flata e2)

norm :: Exp -> Exp
norm = flata . pushNeg

ti3 :: Exp
ti3 = Add ti1 (Neg (Neg ti1))
