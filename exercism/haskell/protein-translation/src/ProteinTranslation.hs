module ProteinTranslation
  ( proteins
  )
where

import qualified Data.Map.Strict as M

codons :: M.Map String (Maybe String)
codons = M.fromList $ do
  (xs, y) <-
    [ ("AUG", Just "Methionine")
      , ("UUU UUC", Just "Phenylalanine")
      , ("UUA UUG", Just "Leucine")
      , ("UCU UCC UCA UCG", Just "Serine")
      , ("UAU UAC", Just "Tyrosine")
      , ("UGU UGC", Just "Cysteine")
      , ("UGG", Just "Tryptophan")
      , ("UAA UAG UGA", Nothing)
      ]
  x <- words xs
  pure (x, y)

proteins :: String -> Maybe [String]
proteins [] = pure []
proteins xs = do
  let (cs, xs') = splitAt 3 xs
  mayCodon <- codons M.!? cs
  case mayCodon of
    Nothing -> pure []
    Just x -> (x :) <$> proteins xs'
