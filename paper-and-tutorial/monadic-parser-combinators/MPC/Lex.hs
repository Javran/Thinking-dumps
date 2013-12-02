module MPC.Lex
where

import MPC.Core
import Control.Monad

-- any of ' ', '\n', '\t'
spaces :: Parser ()
spaces = do
    many1 (sat isSpace)
    return ()
    where
        isSpace x = x `elem` " \n\t"

-- anything begins with "--"
comment :: Parser ()
comment = do
    string "--"
    many $ sat (/= '\n')
    -- you might notice that '\n' is left 
    --   and I think that is on purpose
    --   '\n' should be consumed by `space` parser.
    return ()

-- any content between "{-" and "-}"
--   could be nested
multilineComment :: Parser ()
multilineComment = do
    bracket (string "{-")
            content
            (string "-}")
    return ()
    where
        content = many $ multilineComment +++ notCommentEnd
        -- notCommentEnd should either consume some input or fail
        --   or `many` might not have a chance to terminate.
        notCommentEnd = do
            notParser $ string "-}"
            -- if the following stuff is not a end comment,
            --   one character can be consumed safely
            item
            return ()

-- succeeds when `p` fails,
-- fails when `p` succeeds.
--   note here `notParser` either succeed *without consuming anything*
--     or fail.
notParser :: Parser a -> Parser ()
notParser p = Parser $ \inp ->
    runParser (newParser inp) inp
    where
        newParser inp' =
            case runParser p inp' of
                -- rejected by p
                [] -> result ()
                -- accepted by p
                _  -> zero

-- consume spaces or comments
junk :: Parser ()
junk = do
    many (spaces +++ comment +++ multilineComment)
    return ()

-- removes junk before applying `p`
parse :: Parser a -> Parser a
parse p = junk >> p

-- removes junk after applying `p`
token :: Parser a -> Parser a
token p = do
    v <- p
    junk
    return v

-- parse natual numbers
natural :: Parser Int
natural = token int

-- parse integers
integer :: Parser Int
integer = token int

-- parse symbol (a string)
symbol :: String -> Parser String
symbol = token . string

-- parse identifier, should not contain keywords
identifier :: [String] -> Parser String
identifier ks = token $ do
    x <- ident
    guard (x `notElem` ks)
    return x
