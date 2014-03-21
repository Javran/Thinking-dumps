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
    let lasts = universe :: [LastName]
        firsts = universe :: [FirstName]
    -- lXXX : XXX 's last name is lXXX
    -- (father is determined by his last name)

    lMaryAnn   <- lasts
    lLorna     <- lasts
    lMelissa   <- lasts
    lRosalind  <- lasts
    lGabrielle <- lasts

    yMoore   <- firsts
    yDowning <- firsts
    yHall    <- firsts
    yHood    <- firsts
    yParker  <- firsts

    guard $ distinct [lMaryAnn, lLorna, lMelissa, lRosalind, lGabrielle]
    guard $ distinct [yMoore, yDowning, yHall, yHood, yParker]

    let yOwns = [ (Moore, yMoore)
                , (Downing, yDowning)
                , (Hall, yHall)
                , (Hood, yHood)
                , (Parker, yParker) ]

    let fatherIs =  [ (lMaryAnn, MaryAnn)
                    , (lLorna, Lorna)
                    , (lMelissa, Melissa)
                    , (lRosalind, Rosalind)
                    , (lGabrielle, Gabrielle)
                    ]

    guard $ lMaryAnn == Moore
    guard $ yMoore /= MaryAnn

    guard $ yHood == Gabrielle
    guard $ lGabrielle /= Hood

    guard $ yMoore == Lorna
    guard $ lLorna /= Moore

    guard $ yHall == Rosalind
    guard $ lRosalind /= Hall

    guard $ yDowning == Melissa
    guard $ lMelissa /= Downing

    guard $ lMelissa == Hood

    let (Just dauOfParker) = lookup lGabrielle yOwns
        (Just firstName) = lookup Parker fatherIs

    guard $ dauOfParker == firstName
    guard $ lGabrielle /= Parker

    return fatherIs

main :: IO ()
main = print solutions
