pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t)
 | null grouped = [[h]]
 | head (head grouped) == h = (h:head grouped):tail grouped
 | otherwise = [h]:grouped
    where grouped = pack t

main :: IO ()
main = print (pack "aaaabccaadeeee")
