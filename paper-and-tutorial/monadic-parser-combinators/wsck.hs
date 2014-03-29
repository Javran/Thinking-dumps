-- White-Space, Comments, and Keywords

import System.IO

import MPC.Core
import MPC.Lex

-- codes are moved to MPC.Lex

main = do
    print $
        runParser
            multilineComment
            (unlines
                [ "{- -- comment"
                , " {- nested"
                , "  -}" 
                , " -- comment "
                , "-}code start here"
                ])
    print $
        runParser
            (first junk)
            (unlines
                [ "--comment"
                , "  {- {-  "
                , " -}  "
                , " -}  "
                , "\t\t\t\t   "
                , "code start here"
                ])
    content <- readFile "./wsck.txt"
    print $
        runParser
            (parse $ first $ testParser)
            content
    where
        testParser = do
            a <- natural
            b <- integer
            c <- integer
            let s = a + b + c
            s1 <- symbol "sym1"
            s2 <- symbol "sym3"
            ids <- many $ identifier []
            return (s, [s1,s2], ids)
