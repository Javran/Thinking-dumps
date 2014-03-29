import MPC.Core

colour :: Parser String
colour = p1 +++ p2
    where
        p1 = string "yellow"
        p2 = string "orange"

-- this `many'` acts like `many1`, who is howerver deterministic
--   by using (+++) to combine posibilities
many' :: Parser a -> Parser [a]
many' p = do
    x <- p
    xs <- (many' p +++ return [])
    return (x:xs)

main = do
    -- to show a comparison
    print $ runParser (many1 colour) "yelloworangeyellow"
    print $ runParser (many' colour) "yelloworangeyellow"
