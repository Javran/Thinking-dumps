newtype Parser a = Parser (String -> [(a,String)])

runParser :: Parser a -> String -> [(a,String)]
runParser (Parser p) inp = p inp

result :: a -> Parser a
result v = Parser $ \inp -> [(v,inp)]

zero :: Parser a
zero = Parser $ \inp -> []

item :: Parser Char
item = Parser $ \inp ->
    case inp of
         [] -> []
         (x:xs) -> [(x,xs)]

instance Monad Parser where
    return = result
    (Parser m) >>= f = Parser $ \inp -> do
        (v,inp') <- m inp
        let (Parser newM) = f v
        newM inp'

inBetween :: (Ord a) => a -> a -> a -> Bool
inBetween a b v = a <= v && v <= b

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
       then return x
       else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

digit :: Parser Char
digit = sat $ inBetween '1' '9'

lower :: Parser Char
lower = sat $ inBetween 'a' 'z'

upper :: Parser Char
upper = sat $ inBetween 'A' 'Z'

plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ f
    where 
        f inp = m1 ++ m2
            where m1 = runParser p inp
                  m2 = runParser q inp

letter :: Parser Char
letter = lower `plus` upper

main = do
    let result = runParser (letter >>= \x -> digit >>= \y -> return [x,y]) "A9C"
    print result
