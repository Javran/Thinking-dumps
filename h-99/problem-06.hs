import Control.Arrow

-- keep track of a pair of indices,
-- `fst` part from left to right
-- `snd` part from right to left
-- compare the element that both part of the pair points to

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = all (uncurry (==)) pairValues
    where pairs = takeWhile (uncurry (<=)) $
                      iterate ((+ 1) *** subtract 1) (0, length xs-1)
          pairValues = map (both (xs !!)) pairs
          both f = f *** f

main :: IO ()
main = do
    let v1,v3 :: [Int]
        v1 = [1,2,3]
        v2 = "madamimadam"
        v3 = [1,2,4,8,16,8,4,2,1]

    print $ isPalindrome v1
    print $ isPalindrome v2
    print $ isPalindrome v3
