import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["firmware"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        liftIO $ removeFiles "" [
            "*/cabal.sandbox.config"
          , "*/.ghc.environment.*"
          , "*/dist-newstyle"
          , "*/dist"
          ]
        removeFilesAfter "_build" ["//*"]

    phony "csource-clean" $ do
        liftIO $ removeFiles "_build/csource" ["//*"]

    phony "csource" $ do
        cmd (Cwd "ionic") "nix-shell ../ionic-shell.nix --run"
            ["cabal new-run -- ionic ../_build/csource"]

    "firmware" %> \out -> do
        need ["csource"]
        cs <- getDirectoryFiles "" ["//_build/csource/*.c"]
        let os = [c -<.> "o" | c <- cs]
        need os
        () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc -o" ["_build" </> out] os
        pure ()

    "_build/csource/*.o" %> \out -> do
        let c = out -<.> "c"
        let m = out -<.> "m"
        () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc -c"
              [c] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m
