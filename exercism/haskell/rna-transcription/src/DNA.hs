module DNA
  ( toRNA
  )
where

type DNucleo =
  Char -- ^ nucleotides

type RNucleo = Char

type DNA = [DNucleo]

type RNA = [RNucleo]
-- ^ transcribe DNA to RNA
-- see also: http://en.wikipedia.org/wiki/Transcription_(genetics)

toRNA :: DNA -> Either Char RNA
toRNA = mapM toRNucleo
-- ^ convert DNucleo to RNucleo

toRNucleo :: DNucleo -> Either Char RNucleo
toRNucleo x = case lookup x table of
  Just v -> pure v
  Nothing -> Left x
  where
    table =
      [ ('A', 'U')
      , ('T', 'A')
      , ('G', 'C')
      , ('C', 'G')
      ]
