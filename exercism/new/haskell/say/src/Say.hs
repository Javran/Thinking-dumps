module Say
  ( inEnglish
  ) where

inEnglish :: Integral a => a -> Maybe String
inEnglish 0 = Just "zero"
inEnglish v = do
    -- v is always greater than 0,
    -- otherwise it's captured by the first case
    ts <- chunksOfThousands v
    let l = length ts
        -- part1 is ___, ___, ___, ???
        -- every number (v > 0) contains part1
        part1 = sayChunk (head ts)
        -- conditionally adding an ending word to the sequence of words
        -- if that sequence is not empty
        addEnding ending xs = case xs of
            [] -> []
            _ -> xs ++ [ending]
        -- there are some redundancy here but I don't think it worth the effort
        -- to make it look more compact
        -- part2 is ___, ___, ???, ___
        part2 = if l >= 2
                  then addEnding "thousand" (sayChunk (ts !! 1))
                  else []
        -- part3 is ___, ???, ___, ___
        part3 = if l >= 3
                  then addEnding "million" (sayChunk (ts !! 2))
                  else []
        -- part4 is ???, ___, ___, ___
        part4 = if l >= 4
                  then addEnding "billion" (sayChunk (ts !! 3))
                  else []
    return (unwords (concat [part4, part3, part2, part1]))

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
--   also note that when v == 0, an empty list is generated
sayChunk :: Int -> [String]
sayChunk v
    | v == 0 = []
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
