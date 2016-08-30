{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Final where

{-# ANN module "HLint: ignore Eta reduce" #-}

class ExpSYM repr where
    lit :: Int -> repr
    neg :: repr -> repr
    add :: repr -> repr -> repr

instance ExpSYM Int where
    lit = id
    neg n = - n
    add e1 e2 = e1 + e2

instance ExpSYM String where
    lit n = show n
    neg e = "(-" ++ e ++ ")"
    add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"

-- same thing, but in so called "final embedding"
tf1 :: ExpSYM r => r
tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))

view :: String -> String
view = id
