pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t) = if null grouped
               then [[h]]
               else (if head (head grouped) == h
                       then (h:head grouped):tail grouped
                       else [h]:grouped)
    where grouped = pack t

main :: IO ()
main = print (pack "aaaabccaadeeee")
