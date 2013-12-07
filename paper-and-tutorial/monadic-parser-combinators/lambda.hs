-- a parser for lambda expression

import Control.Monad

import MPC.Core
import MPC.Lex

data Expr = App Expr Expr        -- application
          | Lam String Expr      -- lambda abstraction
          | Let String Expr Expr -- local definition
          | Var String           -- variable
          deriving Show

expr :: Parser Expr
expr = atom `chainl1` return App

atom :: Parser Expr
atom = lam +++ local +++ var +++ paren

lam :: Parser Expr
lam = do
    symbol "\\"
    x <- variable
    symbol "->"
    e <- expr
    return $ Lam x e

local :: Parser Expr
local = do
    symbol "let"
    x <- variable
    symbol "="
    e <- expr
    symbol "in"
    e' <- expr
    return $ Let x e e'

var :: Parser Expr
var = liftM Var variable

paren :: Parser Expr
paren = bracket
            (symbol "(")
            expr
            (symbol ")")

-- parse a variable
variable :: Parser String
variable = identifier ["let", "in"]

main = do
    print $
        runParser
            (parse expr)
            -- note "f x x"  is "(f x) x",
            --   corresponding to App (App (Var f) (Var x)) (Var x)
            " {- a Y combinator -}  \\ f -> ( \\x -> f x x )  ( \\ x -> f x x   ) "
    print $
        runParser
            (parse expr)
            " {- least fixed point -} \\f -> let x = f x in x"
