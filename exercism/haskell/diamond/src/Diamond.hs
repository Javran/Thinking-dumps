module Diamond
  ( diamond
  )
where

import Control.Monad
import Data.Char

diamond :: Char -> Maybe [String]
diamond 'A' = Just ["A"]
diamond ch = do
  guard (isAsciiUpper ch)
  let offset = ord ch - ord 'A'
      sidePad0 : sidePads =
        iterate (drop 1) $ replicate offset ' '
      -- first line is special in that 'A' appears only once.
      firstLine = sidePad0 <> "A" <> sidePad0
      triangle =
        firstLine :
        zipWith3
          (\c sidePad midPad ->
             sidePad <> [c] <> midPad <> [c] <> sidePad)
          ['B' .. ch]
          sidePads
          (fmap (\cnt -> replicate cnt ' ') [1, 3 ..])

  pure $ triangle <> reverse (init triangle)
