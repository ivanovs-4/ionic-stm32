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

import Ivored.Helpers
import Ivored.Inc.STM32F10x

import qualified Ivored.Inc.STM32F10x.GPIO as GPIO


compileIonicSchedule :: ScheduleParams -> String -> IO ()
compileIonicSchedule sp targetDir = do
  let ivoryOpts = initialOpts { scErrors = False
                              , srcLocs = True
                              , outDir = Just targetDir
                              }
  ionCompile ivoryOpts (sched_name sp) (ionSchedule sp)

data ScheduleParams = ScheduleParams
  { sched_pilotStep        :: Def ('[] ':-> ())
  , sched_pilotTemperature :: MemArea ('Stored Uint8)
  , sched_name             :: String
  }

ionSchedule :: ScheduleParams -> Ion ()
ionSchedule ScheduleParams{..} = ion "schedule" $ do

  period (111) $ do
      phase 1 $ ivoryEff $ do
          comment "Pilot step"
          call_ sched_pilotStep

  period p $ do

      phase 0 $ ivoryEff $ do
          call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_RESET

      phase 3 $ ivoryEff $ do
          modifyVar sched_pilotTemperature ((.% 30) . (+1))

      phase (round $ fromIntegral p / 1.618) $ ivoryEff $ do
          call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_SET

      where
          p = 377
