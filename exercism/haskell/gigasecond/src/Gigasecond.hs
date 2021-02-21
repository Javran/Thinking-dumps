{-# LANGUAGE NumericUnderscores #-}

module Gigasecond (fromDay) where

import Data.Time.Clock

fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime 1_000_000_000
