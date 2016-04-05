import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["output/index.html"]

    priority 2 $ "output//index.html" %> \out -> do
        srcFiles <- getDirectoryFiles "src" ["//*.elm"]
        let elmFiles = map (\src -> "output" </> (src -<.> "html")) srcFiles
        liftIO $ print elmFiles
        need ["index.md"]
        need elmFiles
        () <- cmd "pandoc" "index.md" "-o" out
        pure ()

    "output//*.html" %> \out -> do
        let src = "src" </> dropDirectory1 (out -<.> "elm")
        () <- cmd "elm-make" src "--output" out
        pure ()

    pure ()
