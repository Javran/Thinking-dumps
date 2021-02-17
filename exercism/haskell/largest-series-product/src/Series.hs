module Series
  ( Error (..)
  , largestProduct
  )
where

import Control.Monad.Except
import Data.Char
import Data.List

data Error
  = InvalidSpan
  | InvalidDigit Char
  deriving (Show, Eq)

digits :: [] Char -> Except Error ([] Int)
digits = mapM $ \ch ->
  if isDigit ch
    then pure (ord ch - ord '0')
    else throwError $ InvalidDigit ch

listSlices :: Int -> [a] -> [[a]]
listSlices slen xs =
  take nPieces
    . map (take slen)
    . tails
    $ cycle xs
  where
    len = length xs
    nPieces = len - slen + 1

slices :: Int -> String -> Except Error [[Int]]
slices n xs = listSlices n <$> digits xs

largestProduct :: Int -> String -> Either Error Integer
largestProduct 0 [] = Right 1
largestProduct n raw = runExcept $ do
  when (null raw || n < 0) $
    throwError InvalidSpan
  products <- map product <$> slices n raw
  when (null products) $
    throwError InvalidSpan
  pure $ fromIntegral $ maximum products
