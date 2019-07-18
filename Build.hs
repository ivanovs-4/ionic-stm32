import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/csource/main.c"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        liftIO $ removeFiles "" [
            ".styx"
          , "cabal.sandbox.config"
          , ".ghc.environment.*"
          , "dist-newstyle"
          , "dist"
          ]
        removeFilesAfter "_build" ["//*"]

    ".styx/shell.nix" %> \out -> do
        cabalfile <- getDirectoryFiles "" ["//*.cabal"]
        need cabalfile
        cmd "sh ./run-in-shell.sh ./styx-shell.nix styx configure"

    "_build/csource/main.c" %> \out -> do
        need [".styx/shell.nix"]
        cmd "sh ./run-in-shell.sh .styx/shell.nix cabal new-run blink-ion _build/csource"

    -- "_build/csource/main" %> \out -> do
    --     need ["_build/csource/main.c"]
    --     cs <- getDirectoryFiles "" ["//_build/csource/*.c"]
    --     let os = [c -<.> "o" | c <- cs]
    --     need os
    --     () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc -o" [out] os
    --     pure ()

    -- "_build/csource/*.o" %> \out -> do
    --     let c = out -<.> "c"
    --     let m = out -<.> "m"
    --     () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc -c" [c] "-o" [out] "-MMD -MF" [m]
    --     needMakefileDependencies m
