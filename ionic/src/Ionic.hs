{- |
Description: Source code of STM32 firmware
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeOperators #-}

module Ionic where

import Control.Monad

import           Data.Word

import           Ivory.Language
import           Ivory.Compile.C.CmdlineFrontend

import           Ivory.Language.Ion.Base
import           Ivory.Language.Ion.Code
import           Ivory.Language.Ion.Operators


printf :: Def ('[IString] :-> Sint32)
printf = importProc "printf" "stdio.h"

simpleSchedule :: Ion ()
simpleSchedule = ion "schedule" $ do

  period 100 $ do
    phase 1 $ ivoryEff $ do
      comment "period 100, phase 1"
