{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-top-binds #-}

module Lib
  ( main
  )
where

import Control.Applicative
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

fpToText :: FilePath -> T.Text
fpToText = either id id . Filesystem.Path.CurrentOS.toText

main :: IO ()
main = sh $ do
  Just repo <- fmap fromText <$> need "HASKELL_REPO"
  Just targetResolver <- need "TARGET_RESOLVER"
  xs <- reduce Fold.list $ do
    stackYaml <- find (suffix "stack.yaml") repo
    let replacer = do
          r <- "resolver: "
          (r <> targetResolver) <$ (many anyChar >> eof)
    inplace replacer stackYaml
  liftIO $ putStrLn $ show (length xs) <> " files visited."
