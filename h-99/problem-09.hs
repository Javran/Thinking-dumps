pack :: Eq a => [a] -> [[a]]
-- packing an empty list results in another empty list
pack [] = []
-- packing a non-empty list is first packing the tail of it
-- and then:
pack (h:t)
 -- if the tail is empty, head forms a list of itself
 | null grouped = [[h]]
 -- otherwise we examine the grouped list
 -- if h is equal to the first group, then we simply insert it in front of it
 | head (head grouped) == h = (h:head grouped):tail grouped
 -- otherwise, h forms a group of itself
 | otherwise = [h]:grouped
    where grouped = pack t

main :: IO ()
main = print (pack "aaaabccaadeeee")
