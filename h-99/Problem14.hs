module Problem14 where

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

main :: IO ()
main = do
     print . dupli $ [1 :: Int, 2, 3]
     print . dupli $ "abccd"
