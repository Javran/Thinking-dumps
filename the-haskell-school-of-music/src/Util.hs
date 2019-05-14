module Util where

import Control.DeepSeq
import Euterpea
import Euterpea.IO.MIDI.MidiIO
import Data.List

{-

  Helper function to find FluidSynth device for us.

  It makes more sense that mkPlay :: IO (forall a. (ToMusic1 a, NFData a) => IO ()),
  but GHC won't support that properly.

  Example:

  HS> p <- mkPlay @Pitch -- bind it to 'p', and use it everywhere.
  Using output device #OutputDeviceID 2
  HS> p t251

 -}
mkPlay :: (ToMusic1 a, NFData a) => IO (Music a -> IO ())
mkPlay = do
  (_, outDevs) <- getAllDevices
  let synthDev = filter isSynthDev outDevs
      isSynthDev (_, DeviceInfo {name=n}) | "Synth input port" `isPrefixOf` n = True
      isSynthDev _ = False
      (dId,_):_ = synthDev
  putStrLn $"Using output device: " <> show dId
  pure (playC defParams{devID = Just dId})
