{-# LANGUAGE TupleSections #-}

module DNA
  ( Nucleotide (..)
  , nucleotideCounts
  )
where

import Control.Monad.Except
import qualified Data.Map.Strict as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

-- | test if the given char is nucleotide
getNucleotide :: Char -> Except String Nucleotide
getNucleotide ch = case reads [ch] of
  [(n, "")] -> pure n
  _ -> throwError "parse error"

-- | count occurences of all nucleotides given a DNA string
nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts xs = runExcept $ do
  ns <- mapM getNucleotide xs
  pure $ M.fromListWith (+) $ fmap (,1) ns
