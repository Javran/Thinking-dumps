{-# LANGUAGE ScopedTypeVariables #-}
module Problem93 where

import Control.Arrow
import Data.Maybe

-- | all possible ways of splitting a list into 2 parts
splits :: [a] -> [([a],[a])]
splits [] = [([],[])]
splits as@(x:xs) = ([],as) : map (first (x:)) (splits xs)

-- | take 2 consecutive elements from a list, with context (left part + right part)
take2s :: forall a. [a] -> [((a,a),([a],[a]))]
take2s xs =
    catMaybes
  . takeWhile isJust
  . map convert
  $ splits xs
  where
    convert :: ([a],[a]) -> Maybe ((a,a),([a],[a]))
    convert (ls,rs) = case rs of
        (r1:r2:rs') -> Just ((r1,r2),(ls,rs'))
        _ -> Nothing
