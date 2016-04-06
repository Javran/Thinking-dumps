import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.List
import Text.Printf

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["output/index.html"]

    phony "clean" $
        removeFilesAfter "output" ["//*"]

    priority 2 $ "output//index.html" %> \out -> do
        srcFiles <- getDirectoryFiles "src" ["//*.elm"]
        let elmFiles = map (\src -> "output" </> (src -<.> "html")) srcFiles
        need ("CopyAssets" : "output/index.md" : elmFiles)
        cmd "pandoc" "output/index.md" "-o" out

    "output/index.md" %> \out -> do
        -- Tools.elm is a collection of helper functions
        -- and has no main entry point, so it's not related to
        -- the task of generating index file
        let notTool = (/= "Tools.elm")
        srcFiles <- filter notTool <$> getDirectoryFiles "src" ["//*.elm"]
        liftIO $ do
            let listItems = map transform
                          . sort
                          $ srcFiles
                transform srcFile = printf "* [%s](%s)" item htmlPath
                  where
                    item = dropExtension srcFile
                    htmlPath = "/" ++ srcFile -<.> "html"
            print srcFiles
            writeFile out (unlines listItems)

    "output//*.html" %> \out -> do
        let src = "src" </> dropDirectory1 (out -<.> "elm")
        need [src]
        cmd "elm-make" src "--output" out

    "output/img/*" %> \out -> do
        let src = dropDirectory1 out
        need [src]
        cmd "cp" src out

    phony "CopyAssets" $ do
        srcFiles <- getDirectoryFiles "img" ["//*"]
        need (("output/img" </>) <$> srcFiles)
