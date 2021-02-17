{-# LANGUAGE TupleSections #-}

module PigLatin
  ( translate
  )
where

import Control.Applicative
import Data.List
import qualified Data.Map.Strict as M

{-
  well, those are just random rules. garbage in, garbage out.
-}

-- Using custom ADT rather than Bool to avoid boolean blindness.
data ClusterType = Consonant | Vowel deriving (Eq)

getCluster :: String -> Maybe (String, ClusterType)
getCluster xs = do
  _ : _ <- pure xs -- xs must be non-empty
  foldr (\k r -> fmap (k,) (clusterTable M.!? k) <|> r) (Just (take 1 xs, Consonant)) prefixes
  where
    {-
      for "abc", prefixes are "abc", "ab", "a" (up to maxLen),
      we need to lookup clusters in this particular order in order to
      always prefer the longest prefix.
     -}
    prefixes = reverse . take maxLen . tail . inits $ xs

    clusterTable :: M.Map String ClusterType
    clusterTable =
      M.fromList $ fmap (,Vowel) vowels <> fmap (,Consonant) consonants
    maxLen = maximum $ fmap length (vowels <> consonants)
    vowels = words "xr yt a e i o u"
    consonants = words "sch thr ch qu th rh y"

translate :: String -> String
translate = unwords . fmap translateWord . words

translateWord :: String -> String
translateWord w = case getCluster w of
  Just (_, Vowel) -> w <> "ay"
  Just (x, Consonant)
    | y <- drop (length x) w ->
      case y of
        'q' : 'u' : zs -> zs <> x <> "quay"
        'y' : zs -> 'y' : zs <> x <> "ay"
        _ -> drop (length x) w <> x <> "ay"
  Nothing | x : xs <- w -> xs <> [x] <> "ay"
  _ -> error "Unknown initial cluster."
