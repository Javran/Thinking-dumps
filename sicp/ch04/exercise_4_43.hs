import Control.Monad
import Data.List

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

solutions :: Bool -> [[ (LastName, FirstName) ]]
solutions q2 = do
    let lasts = universe
        firsts = universe

    -- lFIRST : FIRST's last name is lFIRST
    -- (father is determined by his last name)
    -- yLAST : LAST owns the yacht named yLAST

    let yHood     = Gabrielle
        yMoore    = Lorna
        yHall     = Rosalind
        yDowning  = Melissa
        lMelissa  = Hood

    lMaryAnn   <- if q2 then lasts else [Moore]

    lLorna     <- delete Moore lasts
    lRosalind  <- delete Hall  lasts
    lGabrielle <- delete Hood  lasts
    yParker    <- firsts

    let yOwns = -- <who> owns <yacht name>
            [ (Moore,   yMoore)
            , (Downing, yDowning)
            , (Hall,    yHall)
            , (Hood,    yHood)
            , (Parker,  yParker) ]

        daughterIs = -- <lastname>'s daughter is <firstname>
            [ (lMaryAnn,   MaryAnn)
            , (lLorna,     Lorna)
            , (lMelissa,   Melissa)
            , (lRosalind,  Rosalind)
            , (lGabrielle, Gabrielle)
            ]
        fullName = daughterIs -- daughter's full name

    guard . distinct . map snd $ yOwns
    guard . distinct . map fst $ daughterIs

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
    return fullName

main :: IO ()
main = do
    putStrLn "Question 1"
    mapM_ print (solutions False)
    putStrLn "Question 2"
    mapM_ print (solutions True)
