module Pilot where

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Copilot.Compile.C99
import Language.Copilot
import Prelude hiding ((>), (<), div, not)
import System.Directory


compileCopiloted :: PilotInfo -> String -> IO ()
compileCopiloted pi targetDir = do
    withCurrentDirectory targetDir $ do
        reify (spec pi) >>= compile (pilotInfo_fileName pi)

data PilotInfo = PilotInfo
  { pilotInfo_params   :: PilotParams
  , pilotInfo_actions  :: PilotActions
  , pilotInfo_fileName :: String
  }

data PilotParams = PilotParams
  { pp_temperature  :: String
  }

data PilotActions = PilotActions
  { pa_blinkOn  :: String
  , pa_blinkOff :: String
  }

spec (PilotInfo {pilotInfo_params=PilotParams{..}, pilotInfo_actions=PilotActions{..}}) = do
  -- Triggers that fire when the ctemp is too low or too high,
  -- pass the current ctemp as an argument.
    -- trigger "blinkon"  (ctemp < 18.0) [arg ctemp]
    -- trigger "blinkoff" (ctemp > 21.0) [arg ctemp]
    trigger pa_blinkOn  (ctemp < 18.0) []
    trigger pa_blinkOff (ctemp > 21.0) []

  where

    -- External temperature as a byte, range of -50C to 100C
    -- extern :: Typed a => String -> Maybe [a] -> Stream a
    temp :: Stream Word8
    temp = extern pp_temperature Nothing

    -- -- externFun :: Typed a => String -> [Arg] -> Maybe (Stream a) -> Stream a
    -- setPIN :: Stream Word8
    -- setPIN = externFun "GPIO_WriteBit" [] Nothing
    -- -- GPIO_WriteBit(GPIOC, GPIO_Pin_13, Bit_SET)

    -- Calculate temperature in Celcius.
    -- We need to cast the Word8 to a Float. Note that it is an unsafeCast, as there
    -- is no direct relation between Word8 and Float.
    ctemp :: Stream Float
    ctemp = (unsafeCast temp) * (150.0 / 255.0) - 50.0
