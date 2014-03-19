import Control.Monad (guard)

-- either one is true
xor :: Bool -> Bool -> Bool
xor = (/=)

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

solutions :: [[(String, Int)]]
solutions = do
    let places = [1..5]
        (who1, place1) `gXor` (who2, place2) =
            guard $ (who1 == place1) `xor` (who2 == place2)

    betty <- places
    ethel <- places
    joan  <- places
    kitty <- places
    mary  <- places

    guard $ distinct [betty, ethel, joan, kitty, mary]

    -- Betty: Kitty was second, I was only third.
    (kitty, 2) `gXor` (betty, 3)

    -- Ethel: I was top. Joan was 2nd
    (ethel, 1) `gXor` (joan, 2)

    -- Joan: I was third, Ethel was bottom
    (joan, 3) `gXor` (ethel, 5)

    -- Kitty: I came out second, Mary was only fourth
    (kitty, 2) `gXor` (mary, 4)

    -- Mary: I was fourth, top place was taken by Betty
    (mary, 4) `gXor` (betty, 1)

    return [ ("Betty", betty)
           , ("Ethel", ethel)
           , ("Joan",  joan)
           , ("Kitty", kitty)
           , ("Mary",  mary)
           ]

main :: IO ()
main = print solutions
