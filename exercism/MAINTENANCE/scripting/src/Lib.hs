{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-top-binds #-}

module Lib
  ( main
  )
where

import Control.Applicative
import qualified Control.Foldl as Fold
import Control.Monad
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS
import System.Environment
import System.Exit hiding (die)
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)

fpToText :: FilePath -> T.Text
fpToText = either id id . Filesystem.Path.CurrentOS.toText

ltsUpdater :: IO ()
ltsUpdater = sh $ do
  Just repo <- fmap fromText <$> need "HASKELL_REPO"
  Just targetResolver <- need "TARGET_RESOLVER"
  xs <- reduce Fold.list $ do
    stackYaml <- find (suffix "stack.yaml") repo
    let replacer = do
          r <- "resolver: "
          (r <> targetResolver) <$ (many anyChar >> eof)
    inplace replacer stackYaml
  liftIO $ putStrLn $ show (length xs) <> " files visited."

runAllTests :: IO ()
runAllTests = sh $ do
  let skipping = S.empty
  Just repo <- fmap fromText <$> need "HASKELL_REPO"
  exerPath <- ls repo
  let exerName = fpToText $ filename exerPath
  marker <- testfile $ exerPath </> "MIGRATION_MARKER"
  when (marker && S.notMember exerName skipping) $ do
    pushd exerPath
    liftIO $ T.putStrLn $ "Testing " <> exerName
    _ <- procStrict "ew" ["fmt"] ""
    (ec, out) <- procStrict "ew" ["test"] ""
    unless (ec == ExitSuccess) $ do
      liftIO $ T.putStrLn out
      die "Test failed."

main :: IO ()
main =
  getArgs >>= \case
    "ltsupdate" : _ -> ltsUpdater
    "testmigrate" : _ -> runAllTests
    _ -> pure ()
