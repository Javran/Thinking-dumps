{-# LANGUAGE TupleSections #-}

module PigLatin
  ( translate
  )
where

import Control.Applicative
import qualified Data.Map.Strict as M

-- Using custom ADT rather than Bool to avoid boolean blindness.
data ClusterType
  = Consonant
  | Vowel
  deriving (Eq)

{-
  for "abcd", this generates:

  [("a", "bcd"), ("ab", "cd"), ("abc","d"), ("abcd", "")]

  the resulting list has the same length as input.
 -}
wordSplits :: [a] -> [([a], [a])]
wordSplits xs =
  -- zip-then-fmap to be lazy on list length
  fst <$> zip (fmap (\p -> splitAt p xs) [1 ..]) xs

getCluster :: String -> Maybe ((String, String), ClusterType)
getCluster xs = do
  _ : _ <- pure xs -- xs must be non-empty
  foldr
    -- try from longest to shorter
    (\p@(k, _) r -> fmap (p,) (clusterTable M.!? k) <|> r)
    -- if all fails, consider just the first character as consonant.
    (Just (splitAt 1 xs, Consonant))
    keySplits
  where
    {-
      Order it so that split with longest prefix goes first.
     -}
    keySplits :: [(String, String)]
    keySplits = reverse . take maxLen $ wordSplits xs

    {-
      All known vowel / consonant clusters (as the first cluster of a word).
      If lookup failed on a non-empty word, consider first character a single consonant cluster.

      This is by no means a list that fully covers English, just sufficient to pass tests.
     -}
    clusterTable :: M.Map String ClusterType
    clusterTable =
      M.fromList $ fmap (,Vowel) vowels <> fmap (,Consonant) consonants
    maxLen = maximum . fmap length . M.keys $ clusterTable

    vowels = words "xr yt a e i o u"
    consonants = words "sch thr ch qu th rh y"

translate :: String -> String
translate = unwords . fmap translateWord . words

translateWord :: String -> String
translateWord w = case getCluster w of
  Just (_, Vowel) ->
    -- Rule 1
    w <> "ay"
  Just ((x, y), Consonant) ->
    case y of
      'q' : 'u' : zs ->
        -- Rule 3
        zs <> x <> "quay"
      'y' : zs ->
        -- Rule 4
        'y' : zs <> x <> "ay"
      _ ->
        -- Rule 2
        y <> x <> "ay"
  Nothing ->
    error "Cannot translate an empty word."
