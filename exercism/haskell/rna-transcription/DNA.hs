module DNA
where

-- ^ personally I think it's a bad name ...
toRNA :: String -> String
toRNA = map (toRNAComplement . fromDNA)

-- ^ convert DNA char to its RNA counterpart
fromDNA :: Char -> Char
fromDNA 'T' = 'U'
fromDNA  r  =  r

-- ^ convert RNA to its complement
toRNAComplement :: Char -> Char
toRNAComplement x = case x of
    'A' -> 'U'
    'U' -> 'A'
    'G' -> 'C'
    'C' -> 'G'
    _   -> error "Invalid RNA char"
