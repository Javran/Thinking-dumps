module Matrix
  ( Matrix
  , row, column
  , rows, cols
  , shape
  , transpose
  , reshape
  , flatten
  , fromString
  , fromList
  ) where

import qualified Data.Vector as V
data Matrix a

row, column :: Int -> Matrix a -> V.Vector a

row = undefined
column = undefined

rows, cols :: Matrix a -> Int

rows = undefined
cols = undefined

shape :: Matrix a -> (Int, Int)
shape = undefined

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape = undefined

transpose :: Matrix a -> Matrix a
transpose = undefined

flatten :: Matrix a -> V.Vector a
flatten = undefined

fromString :: Read a => String -> Matrix a
fromString = undefined

fromList :: [[a]] -> Matrix a
fromList = undefined
