module DNA
    ( toRNA
    )
where

type DNucleo = Char -- ^ nucleotides
type RNucleo = Char

type DNA = [DNucleo]
type RNA = [RNucleo]

-- ^ transcribe DNA to RNA
-- see also: http://en.wikipedia.org/wiki/Transcription_(genetics)
toRNA :: DNA -> RNA
toRNA = map toRNucleo

-- ^ convert DNucleo to RNucleo
toRNucleo :: DNucleo -> RNucleo
toRNucleo x = case x of
    'A' -> 'U'
    'T' -> 'A'
    'G' -> 'C'
    'C' -> 'G'
    _   -> error "Invalid DNucleo"
