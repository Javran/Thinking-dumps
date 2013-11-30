import Control.Monad
import Data.Char
import Data.Maybe

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
    guard $ p x
    return x

-- consume an exact `x`
char :: Char -> Parser Char
char x = sat (\y -> x == y)

-- test if an Ord is within a given range
inBetween :: (Ord a) => a -> a -> a -> Bool
inBetween a b v = a <= v && v <= b

-- consume a digit
digit :: Parser Char
digit = sat $ inBetween '0' '9'

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
{-
-- definition #1
word = newWord `plus` result ""
    where
        newWord = do
            x <- letter
            xs <- word
            return (x:xs)
-}
-- definition #2
word = many letter

-- we have MonadPlus here
--   and I think this is equivalent to 
--   "MonadOPlus" in the paper
instance MonadPlus Parser where
    mzero = zero
    mplus = plus

-- consume a string from input
string :: String -> Parser String
string ""     = return ""
string (x:xs) = do
    char x
    string xs
    return (x:xs)

-- >>>> 4.1 simple repetition
-- consume some chars recognized by `p`
-- refactor: `many` and `many1` can be defined muturally recursively.
many :: Parser a -> Parser [a]
many p = many1 p `plus` return []

-- identifiers are lower-case letter followed by
--   zero or more alphanum
ident :: Parser String
ident = do
    x <- lower
    xs <- many alphanum
    return (x:xs)

-- same as `many`, but this time we don't produce extra empty seq
many1 :: Parser a -> Parser [a]
many1 p = do
    x <- p
    xs <- many p
    return (x:xs)

-- convert string to int, please make sure `(not $ null xs)`
stringToInt :: String -> Int
stringToInt xs = foldl1 merge $ map toInt xs
    where
        toInt x = ord x - ord '0'
        merge a i = a * 10 + i

-- recognize a natural number
nat :: Parser Int
nat = do
    xs <- many1 digit
    return $ stringToInt xs

int :: Parser Int
int = do
    f <- op
    n <- nat
    return $ f n
    where
        op = (char '-' >> return negate) `plus` return id

main = do
    let result = runParser int "-4324Alpha1023 Yes!"
    print result
