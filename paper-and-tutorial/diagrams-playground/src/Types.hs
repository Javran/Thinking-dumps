{-# LANGUAGE
    NoMonomorphismRestriction
  , FlexibleInstances
  , TypeFamilies
  , UndecidableInstances
  #-}
module Types
  ( module Types
  , module Diagrams.Backend.Cairo.CmdLine
  ) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine
import Control.Arrow

instance Mainable (Actioned Double) where
    type MainOpts (Actioned Double)
        = (MainOpts (QDiagram Cairo V2 Double Any), DiagramMultiOpts)
    mainRender x (Actioned y) = defaultMultiMainRender x y

newtype Actioned n = Actioned [(String,IO (QDiagram Cairo V2 n Any))]

instance Monoid (Actioned n) where
    mempty = Actioned []
    (Actioned xs) `mappend` (Actioned ys) = Actioned (xs <> ys)

instance Semigroup (Actioned n) where
    (Actioned xs) <> (Actioned ys) = Actioned (xs <> ys)

nest :: String -> Actioned a -> Actioned a
nest prefix (Actioned xs) = Actioned ((fmap . first) addPrefix xs)
  where
    addPrefix n = prefix ++ "." ++ n
