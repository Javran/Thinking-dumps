module Say
  ( inEnglish
  ) where

inEnglish :: Integral a => a -> Maybe String
inEnglish = undefined

-- | break a value into chunks of thousands
--   note that in result the chunks are in reversed order
--   so 'chunksOfThousands 1234567890' becomes '[890,567,234,1]'
chunksOfThousands :: Integral a => a -> Maybe [Int]
chunksOfThousands v
    | v == 0 = Just [0]
    | v < 0 = Nothing
    | otherwise =
        -- valid ranges: v == 0 (captured by previous case)
        -- v = 1 ~ 999,999,999,999
        -- for a value to be in range, it must be broken into
        -- up to 4 chunks
        let chunks :: [Int]
            chunks =
                  map (fromIntegral . snd)
                . takeWhile (\(x,y) -> x /= 0 || y /= 0)
                . tail
                . iterate (\(x,_) -> x `quotRem` 1000)
                $ (v,0)
        in if length chunks <= 4
             then Just chunks
             else Nothing
