{-# LANGUAGE FlexibleContexts    #-}

import Data.Functor
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
          , native <> "//*.o"
          , native <> "//*.m"
          ]
        removeFilesAfter "_build" ["//*"]

    phony "csource-clean" $ do
        liftIO $ removeFiles "_build/csource" ["//*"]

    phony "csource" $ do
        need [csourceMain]

    csourceMain %> \out -> do
        alwaysRerun
        cmd_ (Cwd "ionic") "nix-shell ../ionic-shell.nix --run"
            ["cabal -O0 new-build ionic"]
        cmd_ (Cwd "ionic") "nix-shell ../ionic-shell.nix --run"
            ["cabal -O0 new-run -- ionic ../_build/csource"]
        -- Dirty hack to add import to generated `ionicSchedule.h`
        -- Since `ion` seemingly does not allow it
        cmd (Cwd "_build/csource") Shell
            "sed -i -Ee 's/(#include \"ivory.h\")/\\1\\n#include \"main.h\"/' ionicSchedule.h"

    -- For this to work you have to create `udev` rules like in ./nix/stlink.nix
    -- For nixos users: add import of ./nix/stlink.nix to /etc/nixos/configuration.nix
    -- and add user to `plugdev` group
    phony "burn" $ do
        need [firmware_bin]
        cmd "nix-shell ./stlink-shell.nix --run"
          [ "st-flash write " <> firmware_bin <> " 0x8000000" ]

    phony "firmware" $ do
        need [firmware_bin]

    firmware_bin %> \out_bin -> do
        need [csourceMain]
        let out_elf = out_bin -<.> "elf"

        native_cs <- fmap mconcat . sequence $ [
              getDirectoryFiles "" [
                native </> "*.c"  -- here is only startup_stm32f10x.c
                -- # Search path for perpheral library
              , core <> "//*.c"
              , device <> "//*.c"
              ]
            , getDirectoryFiles "" $ ((periph </> "src") </>) <$> periphCsUsed
            ]
        let native_os = native_cs <&> (\c -> "_build/csource/native" </> c -<.> "o")
        need native_os

        generated_cs <- getDirectoryFiles "" [ "_build/csource/*.c" ]
        let generated_os = generated_cs <&> (-<.> "o")
        need generated_os

        cmd_ "sh ./run-in-shell.sh ./gcc-shell.nix"
            ([ "arm-none-eabi-gcc"
            , "-o", out_elf
            , "-T" <> ldscript, "-mthumb", "-mcpu=cortex-m3"
            ]
            <> generated_os
            <> native_os
            )
        cmd "sh ./run-in-shell.sh ./gcc-shell.nix"
            "arm-none-eabi-objcopy -O binary"
            [ out_elf, out_bin ]

    "_build/csource/native//*.o" %> \out -> mkObj
        ((joinPath . drop 3 . splitPath $ out) -<.> "c") out []

    "_build/csource/*.o" %> \out -> mkObj (out -<.> "c") out [
              "-I" <> "_build/csource"
            -- , "-D" <> "USE_FULL_ASSERT"
            ]

  where
    mkObj c out flags = do
        let m = out -<.> "m"
        cmd_ "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc"
            "-c" [c] "-o" [out] "-MMD -MF" [m]
            ([ "-mcpu=cortex-m3", "-mthumb", "-nostdlib"
            , "-Os"
            , "-I" <> native
            , "-I" <> core
            , "-I" <> device
            , "-I" <> periph </> "inc"
            , "-D" <> "STM32F10X_MD_VL"
            , "-D" <> "USE_STDPERIPH_DRIVER"
            ] <> flags)
        needMakefileDependencies m

    firmware_bin = "_build/csource/firmware.bin"
    csourceMain = "_build/csource/main.c"

    native = "csource"
    ldscript = native </> "stm32f100.ld"
    libroot = native </> "STM32F10x_standard_libraries/STM32F10x_StdPeriph_Lib_V3.5.0"
    core = libroot </> "Libraries/CMSIS/CM3/CoreSupport"
    device = libroot </> "Libraries/CMSIS/CM3/DeviceSupport/ST/STM32F10x"
    periph = libroot </> "Libraries/STM32F10x_StdPeriph_Driver"
    periphCsUsed = [
        "stm32f10x_gpio.c"
      , "stm32f10x_rcc.c"
      ]
