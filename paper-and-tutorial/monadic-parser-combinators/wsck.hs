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
        -- notCommentEnd should either consume some input or fail
        --   or `many` might not have a chance to terminate.
        notCommentEnd = do
            notParser $ string "-}"
            -- if the following stuff is not a end comment,
            --   one character can be consumed safely
            item
            return ()

-- note here `notParser` either succeed *without consuming anything*
--   or fail.
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
