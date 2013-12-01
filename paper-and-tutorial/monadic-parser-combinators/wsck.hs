-- White-Space, Comments, and Keywords

import MPC.Core

spaces :: Parser ()
spaces = do
    many1 (sat isSpace)
    return ()
    where
        isSpace x = x `elem` " \n\t"

comment :: Parser ()
comment = do
    string "--"
    many $ sat (/= '\n')
    -- I try to consume the last '\n' here
    char '\n'
    return ()

multilineComment :: Parser ()
multilineComment = bracket (string "{-") content (string "-}") >> return ()
    where
        content = many $ multilineComment +++ notCommentEnd
        notCommentEnd = do
            sat (/='-') +++ (sat (=='-') >> sat (/='}'))
            return ()

main = print $
    runParser multilineComment "{-c-}start"
