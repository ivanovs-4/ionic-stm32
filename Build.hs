import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want ["_build/main/main"]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        removeFilesAfter "_build" ["//*"]

    ".styx/shell.nix" %> \out -> do
        cabalfile <- getDirectoryFiles "" ["//*.cabal"]
        need cabalfile
        cmd "sh ./run-in-shell.sh ./styx-shell.nix styx configure"

    "_build/main/main.c" %> \out -> do
        need [".styx/shell.nix"]
        cmd "sh ./run-in-shell.sh .styx/shell.nix cabal new-run blink-ion _build/main"

    "_build/main/main" %> \out -> do
        need ["_build/main/main.c"]
        cs <- getDirectoryFiles "" ["//_build/main/*.c"]
        let os = [c -<.> "o" | c <- cs]
        need os
        () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix gcc -o" [out] os
        pure ()

    "_build/main/*.o" %> \out -> do
        let c = out -<.> "c"
        let m = out -<.> "m"
        () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix gcc -c" [c] "-o" [out] "-MMD -MF" [m]
        needMakefileDependencies m
