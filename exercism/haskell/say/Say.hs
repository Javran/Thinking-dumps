module Say
  ( inEnglish
  ) where

inEnglish :: Integral a => a -> Maybe String
inEnglish v = do
    ts <- chunksOfThousands v
    -- TODO: zero-handling is wrong
    undefined

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

-- | say a value in English
--   note that for 'sayChunk v' to be correct,
--   it's required that '0 <= v <= 999'
sayChunk :: Int -> [String]
sayChunk v
    | v <  20 = [belowTwenties !! v]
    | v <= 99 =
        -- 20 <= v <= 99
        let (q,r) = v `quotRem` 10
            tenStr = fooTies !! (q - 2)
        in if r == 0
             then [tenStr]
                  -- since we know 0 <= r < 10
                  -- the 'head' operation is safe
             else [tenStr ++ "-" ++ head (sayChunk r)]
    | otherwise =
        -- 100 <= v <= 999
        let (q,r) = v `quotRem` 100
            thouStrs = sayChunk q ++ ["hundred"]
        in if r == 0
             then thouStrs
             else thouStrs ++ sayChunk r
  where
    belowTwenties = words
      "zero \
      \one two three four five \
      \six seven eight nine ten \
      \eleven twelve thirteen fourteen fifteen \
      \sixteen seventeen eighteen nineteen"
    fooTies = words
    -- offset:
    --      0      1     2     3     4       5      6      7
      "twenty thirty forty fifty sixty seventy eighty ninety"
