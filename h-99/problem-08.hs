import Data.Maybe
import Data.List

compress :: Eq a => [a] -> [a]
compress = result . foldl go initState
    where
        -- use `fst` as the result, `snd` as the cache
        initState = ([], Nothing)

        -- observe the cached char
        -- if non-empty, attach it to the head
        -- before returning, reverse it
        result (r,c) = (reverse . maybe r (:r)) c

        go (res,prev) curr =
            maybe
                -- nothing
                (res, Just curr)
                -- compare the current element with the previous one
                (\x -> if x == curr
                           then (res, Just x)
                           else (x:res, Just curr))
                prev

-- if we can use library functions
compress1 :: Eq a => [a] -> [a]
compress1 = map head . group

main :: IO ()
main = do
    let t1 = "aaaabccaadeeee"
    print (compress t1)
    print (compress1 t1)
