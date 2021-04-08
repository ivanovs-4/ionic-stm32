module Schedule where

import Control.Monad
-- import Data.Word
import Ivory.Language
import Ivory.Language.Ion.Base
import Ivory.Language.Ion.Code
import Ivory.Language.Ion.Operators

-- import Ivored.Helpers
import Ivored.Inc.STM32F10x


data ScheduleParams = ScheduleParams
  { sched_name             :: !String
  -- , sched_pilotStep        :: !(Def ('[] ':-> ()))
  -- , sched_pilotTemperature :: !(MemArea ('Stored IBool))
  , sched_matrix_schedule  :: ![IvoryAction ()]
  , sched_period_matrix    :: !Int
  , sched_blink_on         :: !(IvoryAction ())
  , sched_blink_off        :: !(IvoryAction ())
  -- , sched_period_1s        :: !Double
  , sched_period_communicate_usb :: !Int
  , sched_communicate_usb  :: !(IvoryAction ())
  }

ionSchedule :: ScheduleParams -> Ion ()
ionSchedule ScheduleParams{..} = ion "schedule" $ do


  -- period (111) $ do
  --     phase 1 $ ivoryEff $ do
  --         comment "Pilot step"
  --         call_ sched_pilotStep

  period sched_period_matrix $ do
      phase 0 $ do
        forM_ (zip [0..] sched_matrix_schedule) $ \(d, eff) -> do
          delay d $ do
              ivoryEff eff

  period sched_period_communicate_usb $ do
      phase 0 $ do
          ivoryEff sched_communicate_usb

  period (5*4181) $ do
      phase 0 $ do
          ivoryEff sched_blink_off
      phase (5*(4181)-2584) $ do
          ivoryEff sched_blink_on
