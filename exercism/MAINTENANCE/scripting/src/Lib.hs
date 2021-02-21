{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Lib
  ( main
  )
where

import Control.Concurrent
import Control.Applicative
import qualified Control.Foldl as Fold
import Control.Monad
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS hiding (null)
import System.Environment
import System.Exit hiding (die)
import qualified System.Process as Process
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)
import qualified System.IO

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
  let skipping =
        -- set of exercise names that we'll skip.
        S.fromList []
  Just repo <- fmap fromText <$> need "HASKELL_REPO"
  exers <-
    catMaybes
      <$> reduce
        Fold.list
        (do
           exerPath <- ls repo
           let exerName = fpToText $ filename exerPath
               isExerciseName = not ("." `T.isPrefixOf` exerName)
           if isExerciseName && S.notMember exerName skipping
             then pure $ Just (exerName, exerPath)
             else pure Nothing)
  liftIO $ putStrLn $ "Found " <> show (length exers) <> " exercises."
  cap <- liftIO getNumCapabilities
  let testExercise (exerName, exerPath) = do
        let mkCp cmd args =
              (Process.proc cmd args)
                { Process.cwd = Just (encodeString exerPath)
                }
        -- ignore formatting result even if it fails.
        _ <- systemStrictWithErr (mkCp "ew" ["fmt"]) ""
        result@(ec, _, _) <- systemStrictWithErr (mkCp "ew" ["test"]) ""
        liftIO $ do
          putStr (if ec == ExitSuccess then "." else "!")
          System.IO.hFlush System.IO.stdout
        pure (exerName, result)
  -- I'm trying to reduce max-in-flight `stack` calls here, which doesn't seem to help much
  liftIO $ setNumCapabilities (min cap 6)
  tasks <- forM exers $ \p ->
    fork (testExercise p)
  results <- mapM wait tasks
  liftIO $ do
    putStrLn "\n"
    setNumCapabilities cap
  let failedResults = filter (\(_, (ec, _, _)) -> ec /= ExitSuccess) results
  liftIO$
    unless (null failedResults) $ do
      putStrLn $ "Found " <> show (length failedResults) <> " failed results, in following exercises:"
      forM_ failedResults $ \(exerName, (ec, _, _)) -> do
        putStrLn $ T.unpack exerName <> ": " <> show ec

main :: IO ()
main =
  getArgs >>= \case
    "ltsupdate" : _ -> ltsUpdater
    "testall" : _ -> runAllTests
    _ -> pure ()
