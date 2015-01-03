module Problem83 where

import Graph
import Problem80

-- see: http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/
--- http://www.ic.unicamp.br/~meidanis/courses/mc336/2009s2/prolog/problemas/p83.gif
p83 :: FndForm Char (Edge Char)
p83 = FndForm . map (Right . parseEdge) . words $ description
  where
    parseEdge (a:b:_) = Edge a b
    description = "ab bc ce eh hg gf fd da de be dg"

main :: IO ()
main = print (graphFormToAdjForm . fndFormToGraphForm $ p83)
