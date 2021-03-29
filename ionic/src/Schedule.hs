module Schedule where

import Control.Monad
import Data.Word
import Ivory.Language
import Ivory.Language.Ion.Base
import Ivory.Language.Ion.Code
import Ivory.Language.Ion.Operators

import Ivored.Helpers
import Ivored.Inc.STM32F10x


data ScheduleParams = ScheduleParams
  { sched_name             :: String
  , sched_pilotStep        :: Def ('[] ':-> ())
  , sched_pilotTemperature :: MemArea ('Stored IBool)
  , sched_checkButtons     :: IvoryAction ()
  -- , sched_blinkOff         :: IvoryAction ()
  }

ionSchedule :: ScheduleParams -> Ion ()
ionSchedule ScheduleParams{..} = ion "schedule" $ do

  -- period (111) $ do
  --     phase 1 $ ivoryEff $ do
  --         comment "Pilot step"
  --         call_ sched_pilotStep

  period p $ do

      phase 0 $ ivoryEff $ do
          sched_checkButtons

      -- phase 3 $ ivoryEff $ do
      --     modifyVar sched_pilotTemperature ((.% 30) . (+1))

      -- phase (round $ fromIntegral p / 1.618) $ ivoryEff $ do
      --     sched_blinkOff

      where
          p = 10
