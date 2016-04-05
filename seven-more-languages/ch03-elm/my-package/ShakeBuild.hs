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
        need ("CopyAssets" : "index.md" : elmFiles)
        cmd "pandoc" "index.md" "-o" out

    "output//*.html" %> \out -> do
        let src = "src" </> dropDirectory1 (out -<.> "elm")
        cmd "elm-make" src "--output" out

    "output/img/*" %> \out -> do
        let src = dropDirectory1 out
        cmd "cp" src out

    phony "CopyAssets" $ do
        srcFiles <- getDirectoryFiles "img" ["//*"]
        need (("output/img" </>) <$> srcFiles)

    pure ()
