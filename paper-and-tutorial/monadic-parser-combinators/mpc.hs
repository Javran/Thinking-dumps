import Control.Monad

newtype Parser a = Parser
    { runParser :: String -> [(a,String)]
    }

-- >>>> 2.2 primitive parsers

-- return `v` without consuming anything
result :: a -> Parser a
result v = Parser $ \inp -> [(v,inp)]

-- always fails
zero :: Parser a
zero = --Parser $ \inp -> []
    Parser $ const []

-- consume the first char, fail if this is impossible
item :: Parser Char
item = Parser $ \inp ->
    case inp of
         []     -> []
         (x:xs) -> [(x,xs)]

-- >>>> 2.3 parser combinators
instance Monad Parser where
    return = result
    m >>= f = Parser $ \inp -> do
        -- apply m on inp, which yields (v, inp')
        (v,inp') <- runParser m inp
        -- apply f on v, which ylelds next parser
        -- run parser on the unconsumned parts inp'
        runParser (f v) inp'

-- consume and test if the first character satisfies
--   a predicate
sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x
       then return x
       else zero

-- consume an exact `x`
char :: Char -> Parser Char
char x = sat (\y -> x == y)

-- test if an Ord is within a given range
inBetween :: (Ord a) => a -> a -> a -> Bool
inBetween a b v = a <= v && v <= b

-- consume a digit
digit :: Parser Char
digit = sat $ inBetween '1' '9'

-- consume a lower case char
lower :: Parser Char
lower = sat $ inBetween 'a' 'z'

-- consume a upper case char
upper :: Parser Char
upper = sat $ inBetween 'A' 'Z'

-- use p and q to parse the same string
--   return all possibilities
plus :: Parser a -> Parser a -> Parser a
p `plus` q = Parser $ \inp ->
    runParser p inp ++ runParser q inp

-- consume letters (i.e. lower / upper)
letter :: Parser Char
letter = lower `plus` upper

-- consume alphanum (i.e. letter / digit)
alphanum :: Parser Char
alphanum = letter `plus` digit

-- consume a word (i.e. consecutive letters)
word :: Parser String
word = newWord `plus` result ""
    where
        newWord = do
            x <- letter
            xs <- word
            return (x:xs)

instance MonadPlus Parser where
    mzero = zero
    mplus = plus

main = do
    let result = runParser word "Yes!"
    print result
