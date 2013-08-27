data Sheep = Sheep String (Maybe Sheep) (Maybe Sheep)
        deriving Show

father :: Sheep -> Maybe Sheep
father (Sheep _ f _) = f

mother :: Sheep -> Maybe Sheep
mother (Sheep _ _ m) = m

maternalGrandfather :: Sheep -> Maybe Sheep
maternalGrandfather s =
    case (mother s) of
         Nothing -> Nothing
         Just m -> father m

mothersParentalGrandfather :: Sheep -> Maybe Sheep
mothersParentalGrandfather s =
    case (mother s) of
         Nothing -> Nothing
         Just m ->
             case (father m) of
                  Nothing -> Nothing
                  Just gf -> father gf

-- Nothing at any point in the computation will cause Nothing to be the final result
--      implement this notion so that all of the explicit `case` testing can be removed

comb :: Maybe a -> (a -> Maybe b) -> Maybe b
-- comb {current result} {next step}
-- * if {current result} has failed, nothing to do
comb Nothing _ = Nothing
-- * result appears to be normal, go on performing next step
comb (Just x) f = f x

-- now we re-impl mothersParentalGrandfather
mothersParentalGrandfather1 :: Sheep -> Maybe Sheep
mothersParentalGrandfather1 s = finalResult
    where
        -- find mother of s
        r1 = comb (Just s) mother
        -- find father of r1
        r2 = comb r1 father
        -- find father of r2
        finalResult = comb r2 father
-- since "op a b" is equal to "a `op` b"
-- and all functions enclosed by graves are left-associative by default,
-- we can rewrite the previous function as:
mothersParentalGrandfather2 :: Sheep -> Maybe Sheep
mothersParentalGrandfather2 s = (Just s) `comb` mother `comb` father `comb` father

-- notice that `comb` has no connection with `Sheep`
--      so it can work together with `Maybe` regardless of the inner type

main = do
        let shpA = Sheep "A" Nothing Nothing
        let shpB = Sheep "B" Nothing Nothing
        let shpC = Sheep "C" (Just shpA) Nothing
        let shpD = Sheep "D" (Just shpC) Nothing
        let shpE = Sheep "E" Nothing (Just shpD)

        print $ father shpC -- shpA
        print $ mother shpC -- Nothing
        print $ maternalGrandfather shpE -- shpC
        print $ mothersParentalGrandfather  shpE -- shpA
        print $ mothersParentalGrandfather1 shpE -- shpA
        print $ mothersParentalGrandfather2 shpE -- shpA
