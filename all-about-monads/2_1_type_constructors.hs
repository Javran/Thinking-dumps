data Maybe1 a = Nothing1 | Just1 a
        deriving Show

main = do
        -- types can be wrapped inside Maybe1
        print $ Just1 "A"
        print $ Just1 1
        print $ (Nothing1 :: Maybe1 ())
        print $ Just1 $ Just1 "A"
