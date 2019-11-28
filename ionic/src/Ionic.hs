{- |
Description: Source code of STM32 firmware
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}

module Ionic where

import Control.Monad
import Data.Word

import Ivory.Compile.C.CmdlineFrontend
import Ivory.Language
import Ivory.Language.Ion.Base
import Ivory.Language.Ion.Code
import Ivory.Language.Ion.Operators

import Ivored.Inc.STM32F10x
import qualified Ivored.Inc.STM32F10x.GPIO as GPIO


compileIonicSchedule targetDir = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Just targetDir
                              }
  ionCompile ivoryOpts "ionicSchedule" ionSchedule

ionSchedule :: Ion ()
ionSchedule = ion "schedule" $ do
  let p = 377
  period p $ do

    phase 0 $ ivoryEff $ do
      comment "GPIO_WriteBit(GPIOC, GPIO_Pin_13, Bit_RESET);"
      call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_RESET

    phase (round $ fromIntegral p / 1.618) $ ivoryEff $ do
      comment "GPIO_WriteBit(GPIOC, GPIO_Pin_13, Bit_SET);"
      call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_SET
