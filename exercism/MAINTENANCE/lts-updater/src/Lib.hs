{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC
    -fno-warn-unused-imports
    -fno-warn-unused-top-binds
  #-}

module Lib
  ( main
  )
where

import qualified Control.Foldl as Fold
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS
import System.Exit
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)
import Control.Applicative

fpToText :: FilePath -> T.Text
fpToText = either id id . Filesystem.Path.CurrentOS.toText

main :: IO ()
main = sh $ do
  Just repo <- fmap fromText <$> need "HASKELL_REPO"
  Just targetResolver <- need "TARGET_RESOLVER"
  stackYaml <- find (suffix "stack.yaml") repo
  inplace ("resolver: " >> (("resolver: " <> targetResolver) <$ (many anyChar >> eof))) stackYaml
  pure ()
