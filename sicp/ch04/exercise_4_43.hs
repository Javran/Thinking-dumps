import Control.Monad
import Data.Maybe
-- <First>  <Last>    <Daughter First name>
--          Moore     Mary Ann (! Lorna)
-- Colonel  Downing   !Melissa
-- ?        Hall      !Rosalind
-- Barnacle Hood      !Gabrielle (Melissa)
-- ?        Parker

-- relations:
-- <x> owns <y> -> <x>'s daughter is not <y>
-- <x> daughterIs <y>

data LastName = Moore
              | Downing
              | Hall
              | Hood
              | Parker
                deriving (Eq, Enum, Bounded, Show)

distinct :: Eq e => [e] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

-- not working, maybe we should also keep track of yacht as well.

solutions :: [[ (String, LastName) ]]
solutions = do
    let lasts = [minBound .. maxBound]
        lXXX `yachtIs` yyy = guard $ lXXX /= yyy
    -- lXXX : XXX 's last name is lXXX
    -- (father is determined by his last name)

    lMaryAnn   <- lasts
    lLorna     <- lasts
    lMelissa   <- lasts
    lRosalind  <- lasts
    lGabrielle <- lasts

    let relations =
            [ ("MaryAnn"   , lMaryAnn   )
            , ("Lorna"     , lLorna     )
            , ("Melissa"   , lMelissa   )
            , ("Rosalind"  , lRosalind  )
            , ("Gabrielle" , lGabrielle )
            ]
        colookup b pairs = fromJust (lookup b (map (\(x,y)->(y,x)) pairs))

    guard $ distinct $ map snd relations

    guard $ lMaryAnn == Moore

    guard $ lGabrielle /= Hood

    guard $ lLorna /= Moore

    guard $ lRosalind /= Hall

    --guard $ lMelissa /= Downing

    guard $ lMelissa == Hood

    let parkerDau = colookup Parker relations

    guard $ "Garbrielle" /= parkerDau
    -- guard $ any (\yyy -> lGabrielle /= yyy && lYYY == Parker) undefined
    -- > if "Lorna" Parker
    -- > co-lookup lYYY will find "Lorna",
    -- > lGabrielle owns "Lorna"
    return relations

main :: IO ()
main = print solutions
