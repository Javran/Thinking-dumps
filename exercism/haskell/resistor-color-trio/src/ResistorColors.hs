{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module ResistorColors
  ( Color (..)
  , Resistor (..)
  , label
  , ohms
  )
where

import Control.Applicative
import Control.Monad
import Data.Maybe

data Color
  = Black
  | Brown
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Violet
  | Grey
  | White
  deriving (Show, Enum, Bounded)

newtype Resistor = Resistor
  { bands :: (Color, Color, Color)
  }
  deriving (Show)

label :: Resistor -> String
label r =
  fromMaybe (show val <> " ohms") $
    mkLabel 1_000_000_000 " gigaohms"
      <|> mkLabel 1_000_000 " megaohms"
      <|> mkLabel 1_000 " kiloohms"
  where
    mkLabel b lbl =
      show (val `quot` b) <> lbl <$ guard (val >= b)
    val = ohms r

ohms :: Resistor -> Int
ohms
  ( Resistor
      ( fromEnum -> a
        , fromEnum -> b
        , fromEnum -> c
        )
    ) = (a * 10 + b) * 10 ^ c
