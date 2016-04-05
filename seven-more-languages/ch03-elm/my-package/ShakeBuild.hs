import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["output/Day3.html"]

    "output//*.html" %> \out -> do
        let src = "src" </> dropDirectory1 (out -<.> "elm")
        () <- cmd "elm-make" src "--output" out
        pure ()

    pure ()
