-- this source implements a simple arithmetic expression parser 
--   discribed in 4.3

import MPC.Core

-- >>>> 4.3 repetition with meaningful separators
expr   :: Parser Int
addop  :: Parser (Int -> Int -> Int)
factor :: Parser Int

-- expr     ::= expr addop factor | factor
-- left-recursion to iteration:
-- expr     ::= factor exprrest
-- exprrest ::= '' | addop factor exprrest 
{-
-- impl #1
expr = do
    x <- factor
    fys <- many $ do
            f <- addop
            y <- factor
            return (f,y)
    return $ foldl
        (\acc (op,y) -> acc `op` y)
        x
        fys
-}
-- impl #2
expr = factor `chainl1` addop

-- addop ::= '+' | '-'
{-
-- impl #1
addop =
    (char '+' >> return (+)) `plus`
    (char '-' >> return (-))
-}
-- impl #2
addop = ops
    [ (char '+', (+))
    , (char '-', (-))
    ]

-- factor ::= nat | '(' expr ')'
factor = nat `plus` bracket (char '(') expr (char ')')

-- take as argument a list of pairs
--   whose `fst` is a parser that recognize some string of type `a`
--   and `snd` is the corresponding result
--   this function produces a parser that try to parse something
--   of type `a` in parallel and return all possible `b`s
ops :: [(Parser a, b)] -> Parser b
ops xs = foldr1 plus [ p >> return op | (p,op) <- xs]

main = do
    let result = runParser expr "1+2-(3+4)"
    print result
