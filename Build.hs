#!/usr/bin/env nix-shell
#!nix-shell ./shell-shake.nix -i runghc

{-# LANGUAGE FlexibleContexts    #-}

import Data.Functor
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [firmware_bin]

    phony "clean" $ do
        putNormal "Cleaning files in _build"
        liftIO $ removeFiles "" [
            "*/cabal.sandbox.config"
          , "*/.ghc.environment.*"
          , "*/dist-newstyle"
          , "*/dist"
          , csource <> "//*.o"
          , csource <> "//*.m"
          ]
        removeFilesAfter "_build" ["//*"]
        -- removeFilesAfter "_shake/.shake" ["//*"]

    phony "mainc" $ do
        need [mainc]

    mainc %> \out -> do
        hsfiles <- getDirectoryFiles "" [ "ionic//*.hs", "ionic/*.cabal" ]
        need hsfiles
        cmd_ (Cwd "ionic") "nix-shell ../ionic-shell.nix --run"
            ["cabal -O0 new-build ionic"]
        cmd_ (Cwd "ionic") "nix-shell ../ionic-shell.nix --run"
            ["cabal -O0 new-run -- ionic ../_build"]
        -- Dirty hack to add import to generated `ionicSchedule.h`
        -- Since `ion` seemingly does not allow it
        cmd (Cwd "_build") Shell
            "sed -i -Ee 's/(#include \"ivory.h\")/\\1\\n#include \"main.h\"/' ionicSchedule.h"

    -- For this to work you have to create `udev` rules like in ./nix/stlink.nix
    -- For nixos users: add import of ./nix/stlink.nix to /etc/nixos/configuration.nix
    -- and add user to `plugdev` group
    phony "burn" $ do
        need [firmware_bin]
        cmd "nix-shell ./stlink-shell.nix --run"
          [ "st-flash write " <> firmware_bin <> " 0x8000000" ]

    firmware_bin %> \out_bin -> do
        need [mainc]
        let out_elf = out_bin -<.> "elf"

        csource_cs <- fmap mconcat . sequence $ [
              getDirectoryFiles "" [
                csource </> "*.c"  -- here is only startup_stm32f10x.c
                -- # Search path for perpheral library
              , cmsis </> "Source" <> "//*.c"
              , device </> "Source" <> "//*.c"
              , usb_fs </> "src" <> "//*.c"
              , csource </> "usb" <> "//*.c"
              ]
            , getDirectoryFiles "" $ ((periph </> "src") </>) <$> periphCsUsed
            ]
        let native_os = csource_cs <&> (\c -> "_build/native" </> dropDirSteps 1 c -<.> "o")
        need native_os

        generated_cs <- getDirectoryFiles "" [ "_build/*.c" ]
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

    "_build/native//*.o" %> \out -> mkObj
        ("csource" </> dropDirSteps 2 out -<.> "c") out []

    "_build/*.o" %> \out -> mkObj (out -<.> "c") out [
              "-I" <> "_build"
            -- , "-D" <> "USE_FULL_ASSERT"
            ]

  where
    mkObj c out flags = do
        let m = out -<.> "m"
        cmd_ "sh ./run-in-shell.sh ./gcc-shell.nix arm-none-eabi-gcc"
            "-c" [c] "-o" [out] "-MMD -MF" [m]
            ([ "-mcpu=cortex-m3", "-mthumb", "-nostdlib"
            , "-Os"
            , "-I" <> csource
            , "-I" <> cmsis </> "Include"
            , "-I" <> device </> "Include"
            , "-I" <> periph </> "inc"
            , "-I" <> usb_fs </> "inc"
            , "-I" <> csource </> "usb"
            , "-D" <> "STM32F10X_MD"
            , "-D" <> "USE_STDPERIPH_DRIVER"
            ] <> flags)
        needMakefileDependencies m

    dropDirSteps n = joinPath . drop n . splitPath

    firmware_bin = "_build/firmware.bin"
    mainc = "_build/main.c"

    csource = "csource"
    ldscript = csource </> "stm32f100.ld"
    libroot = csource </> "Libraries"
    cmsis = libroot </> "CMSIS"
    device = libroot </> "CMSIS/Device/ST/STM32F10x"
    periph = libroot </> "STM32F10x_StdPeriph_Driver"
    usb_fs = libroot </> "STM32_USB-FS-Device_Driver"
    periphCsUsed = [
        "stm32f10x_gpio.c"
      , "stm32f10x_rcc.c"
      , "misc.c"
      , "stm32f10x_exti.c"
      ]
