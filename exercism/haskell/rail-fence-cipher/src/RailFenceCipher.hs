module RailFenceCipher
  ( encode
  , decode
  )
where

import Control.Monad
import qualified Data.DList as DL
import Data.Foldable
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

{-
  TODO: this isn't really functional programming,
  I could imagine to do this in functional style will need to compute some indices
  base on # of rails first, which I don't want to get that involved for now.
 -}

railIndices :: Int -> [Int]
railIndices n = cycle $ [0 .. n -1] <> [n -2, n -3 .. 1]

gEncode :: Int -> [a] -> [a]
gEncode n xs = DL.toList $ mconcat (toList accumulated)
  where
    accumulated = V.create $ do
      v <- VM.replicate n DL.empty
      forM_ (zip (railIndices n) xs) $ \(rInd, ch) ->
        VM.unsafeModify v (<> DL.singleton ch) rInd
      pure v

gDecode :: Int -> [a] -> [a]
gDecode n xs = toList reconstructed
  where
    l = length xs
    reindex = gEncode n [0 .. l -1]
    vs = V.fromListN l xs

    reconstructed = V.create $ do
      v <- VM.unsafeNew l
      forM_ (zip [0 .. l -1] reindex) $ \(i, j) ->
        VM.unsafeWrite v j (vs V.! i)
      pure v

encode :: Int -> String -> String
encode = gEncode

decode :: Int -> String -> String
decode = gDecode
