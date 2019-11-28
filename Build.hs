import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="_build"} $ do
    want [firmware_bin]

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

    -- For this to work you have to create `udev` rules like in ./nix/stlink.nix
    -- For nixos users: add import of ./nix/stlink.nix to /etc/nixos/configuration.nix
    -- and add user to `plugdev` group
    phony "burn" $ do
        need [firmware_bin]
        cmd (Cwd "_build/csource") "nix-shell ../../stlink-shell.nix --run" ["make burn"]

    firmware_bin %> \out -> do
        need ["csource"]
        copyFileChanged "csource/Makefile.src" "_build/csource/Makefile"
        cmd (Cwd "_build/csource") "nix-shell ../../gcc-shell.nix --run" ["make"]

--         cs <- getDirectoryFiles "" ["//_build/csource/*.c"]
--         let os = [c -<.> "o" | c <- cs]
--         need os
--         () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc -o" ["_build" </> out] os
--         pure ()

--     "_build/csource/*.o" %> \out -> do
--         let c = out -<.> "c"
--         let m = out -<.> "m"
--         () <- cmd "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc -c"
--               [c] "-o" [out] "-MMD -MF" [m]
--         needMakefileDependencies m

  where
    firmware_bin = "_build/csource/firmware.bin"
