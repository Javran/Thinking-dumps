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
    return ()

multilineComment :: Parser ()
multilineComment = do
    bracket (string "{-")
            content
            (string "-}")
    return ()
    where
        content = many $ multilineComment +++ notCommentEnd
        notCommentEnd1 = notParser $ string "-}"
        notCommentEnd = do
            -- anything but '-}'
            sat (/='-') +++ (sat (=='-') >> sat (/='}'))
            return ()

notParser :: Parser a -> Parser ()
notParser p = Parser $ \inp ->
    runParser (newParser inp) inp
    where
        newParser inp' =
            case runParser p inp' of
                -- rejected by p
                [] -> return ()
                -- accepted by p
                _  -> zero

main = print $
    runParser
        multilineComment
        (unlines
            [ "{- -- comment"
            , " {- nested"
            , "  -}" 
            , " -- comment "
            , "-}code start here"
            ])
