-- this source code implements a simple arithmetic expression parser 
--   that supports a right-associative exponetiation operation
--   described in 4.3
import MPC.Core

-- expr = term `chainl1` addop
expr = chainl term addop 0

-- term = factor `chainr1` expop
term = chainr factor expop 0

factor = nat `plus` bracket (char '(') expr (char ')')

addop = ops
    [ (char '+', (+))
    , (char '-', (-))
    ]

expop = ops
    [ (char '^', (^))
    ]

main = print $
    runParser expr "2^3^2+(1^2^3+6^2)"
