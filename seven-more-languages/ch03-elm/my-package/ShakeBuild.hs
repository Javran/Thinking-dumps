import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["output/Day3.html"]

    priority 2 $ "output//index.html" %> \out -> do
        need ["index.md"]
        () <- cmd "pandoc" "index.md" "-o" out
        pure ()

    "output//*.html" %> \out -> do
        let src = "src" </> dropDirectory1 (out -<.> "elm")
        () <- cmd "elm-make" src "--output" out
        pure ()

    pure ()
