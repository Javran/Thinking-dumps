import Control.Monad
import Data.Maybe
import Data.List
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

data FirstName = MaryAnn
               | Lorna
               | Melissa
               | Rosalind
               | Gabrielle
                 deriving (Eq, Enum, Bounded, Show)

universe :: (Enum e, Bounded e) => [e]
universe = [minBound .. maxBound]

distinct :: Eq e => [e] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs

-- not working, maybe we should also keep track of yacht as well.

-- last name(father), daughter's first name
solutions :: [[ (LastName, FirstName) ]]
solutions = do
    let lasts = universe
        firsts = universe

    -- lXXX : XXX 's last name is lXXX
    -- (father is determined by his last name)

    let lMaryAnn  = Moore
        yHood     = Gabrielle
        yMoore    = Lorna
        yHall     = Rosalind
        yDowning  = Melissa
        lMelissa  = Hood

    lLorna     <- delete Moore lasts
    lRosalind  <- delete Hall lasts
    lGabrielle <- delete Hood lasts
    yParker    <- firsts

    let yOwns = [ (Moore, yMoore)
                , (Downing, yDowning)
                , (Hall, yHall)
                , (Hood, yHood)
                , (Parker, yParker) ]

        daughterIs =  [ (lMaryAnn, MaryAnn)
                      , (lLorna, Lorna)
                      , (lMelissa, Melissa)
                      , (lRosalind, Rosalind)
                      , (lGabrielle, Gabrielle)
                      ]

    guard $ distinct $ map snd yOwns
    guard $ distinct $ map fst daughterIs

    -- those constraints can be determined before searching
    -- guard $ lMaryAnn == Moore
    -- guard $ yMoore /= MaryAnn
    -- guard $ yHood == Gabrielle
    -- guard $ yMoore == Lorna
    -- guard $ yHall == Rosalind
    -- guard $ yDowning == Melissa
    -- guard $ lMelissa /= Downing
    -- guard $ lMelissa == Hood

    guard $ lookup lGabrielle yOwns == lookup Parker daughterIs
    return daughterIs

main :: IO ()
main = print solutions
