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
