-- >>>> 5.1 left factoring

import MPC.Core

eval :: Parser Int
{-
-- impl #1
eval = add `plus` sub
    where
        -- problem: `x <- nat` is unnecessarily evaluated twice
        add = do
            x <- nat
            char '+'
            y <- nat
            return $ x + y
        sub = do
            x <- nat
            char '-'
            y <- nat
            return $ x - y
-}
{-
-- impl #2
eval = do
    x <- nat
    addRest x `plus` subRest x
    where
        addRest x = do
            char '+'
            y <- nat
            return (x+y)
        subRest x = do
            char '-'
            y <- nat
            return (x-y)
-}
-- impl #3
eval = do
    x <- nat
    f <- ops
        [ (char '+', (+))
        , (char '-', (-))
        ]
    y <- nat
    return $ f x y

main = print $
    runParser eval "123-456"
