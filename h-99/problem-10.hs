import Control.Arrow ((&&&))

-- from problem-09
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (h:t)
 | null grouped = [[h]]
 | head (head grouped) == h = (h:head grouped):tail grouped
 | otherwise = [h]:grouped
    where grouped = pack t

encode :: Eq a => [a] -> [(Int,a)]
encode = map (length &&& head) . pack

main :: IO ()
main = print (encode "aaaabccaadeeee")
