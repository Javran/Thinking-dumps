module Base
  ( Error (..)
  , rebase
  )
where

import Control.Monad.Except
import Data.List
import Data.Tuple

data Error a
  = InvalidInputBase
  | InvalidOutputBase
  | InvalidDigit a
  deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits = runExcept $ do
  when (inputBase <= 1) $
    throwError InvalidInputBase
  when (outputBase <= 1) $
    throwError InvalidOutputBase
  num <-
    foldM
      (\acc i -> do
         when (i < 0 || i >= inputBase) $
           throwError (InvalidDigit i)
         pure $ acc * inputBase + i)
      0
      inputDigits
  pure $
    reverse $
      unfoldr
        (\v ->
           if v == 0
             then Nothing
             else Just $ swap $ v `quotRem` outputBase)
        num
