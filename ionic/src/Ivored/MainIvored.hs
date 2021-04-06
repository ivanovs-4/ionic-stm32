{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE KindSignatures #-}

module Ivored.MainIvored where

-- import Data.Function
import Ivory.Language

import Control.Lens
import Control.Monad (forM, forM_, join)
import Control.Monad.State (evalState, get, modify)
import           Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC
import GHC.Types (Symbol)

import Ivored.Helpers as H
import Ivored.Inc.STM32F10x
-- import Pilot
import Schedule

-- [ivory|
--     struct btn_state
--       { on_off    :: Stored IBool
--       ; lasts_on  :: Stored Uint16
--       ; lasts_off :: Stored Uint16
--       }
--     |]

class CInclude a where
  inc :: a -> ModuleDef

instance CInclude BitAction where
  inc = inclSym

instance IvoryArea b => CInclude (Ref a b) where
  inc = inclSym

instance CInclude Uint16 where
  inc = inclSym

instance CInclude Uint32 where
  inc = inclSym

instance CInclude (Def a) where
  inc = incl

instance IvoryArea a => CInclude (MemArea a) where
  inc = defMemArea

instance IvoryStruct a => CInclude (Proxy (a :: Symbol)) where
  inc = defStruct

cmodule :: ScheduleParams -> Module
cmodule ScheduleParams {..} = package "main" $ do
  inc RCC._APB2PeriphClockCmd
  inc RCC._APB2Periph_GPIOA
  inc RCC._APB2Periph_GPIOB
  inc RCC._APB2Periph_GPIOC
  inc enable
  inc GPIO.structInit
  inc (Proxy :: Proxy "GPIO_InitTypeDef_mock")
  inc GPIO.init
  inc GPIO.bit_SET
  inc GPIO.bit_RESET
  inc GPIO.pin_8
  inc GPIO.pin_9
  inc GPIO.pin_13
  inc GPIO.mode_IPD
  inc GPIO.mode_Out_PP
  inc GPIO.mode_IN_FLOATING
  inc GPIO.speed_2MHz
  inc GPIO.speed_10MHz
  inc GPIO.speed_50MHz
  inc GPIO.writeBit
  inc GPIO.readInputDataBit
  inc gpioA
  inc gpioB
  inc gpioC
  inc sysTick_Config
  inc systemCoreClock
  --
  inc main'
  inc sysTick_Handler
  inc ionicSchedule
  -- inc (Proxy :: Proxy "btn_state")

  inc blinkOn
  inc blinkOff

  inc area_btn_ignore
  inc area_btn_current_state
  inc area_btn_debounce_release
  inc process_raw_btn
  -- inc handle_press
  -- inc handle_release
  inc area_btn_presses

  --

  -- inc assert_failed

  inc usb_ionic_prepare
  inc handle_usb_loop

  where
      ionicSchedule :: Def ('[] ':-> ())
      ionicSchedule = importProc sched_name (sched_name <> ".h")

      sysTick_Handler :: Def ('[] ':-> ())
      sysTick_Handler = proc "SysTick_Handler" $ body $ do
          call_ ionicSchedule


matrix_schedule :: [Ivory eff ()]
matrix_schedule =
  [ forM_ (fst <$> take 1 imatrix)
      $ \(gpioGrpAct, gpioPinAct) -> do
          call_ GPIO.writeBit gpioGrpAct gpioPinAct GPIO.bit_SET
  ] <> join ( do
    zip ((Just . fst <$> drop 1 imatrix) <> [Nothing]) imatrix
      <&> \(mnext, ((gpioGrpAct, gpioPinAct), ipins)) ->
        [ do
            bs <- forM (snd <$> ipins) $ \(gpioGrpPas, gpioPinPas) -> do
                fmap (/=?0) $ call GPIO.readInputDataBit gpioGrpPas gpioPinPas
            call_ GPIO.writeBit gpioGrpAct gpioPinAct GPIO.bit_RESET

            forM_ (zip (fst <$> ipins) bs) $ \(j, b) -> call_ process_raw_btn (fromIntegral j) b

            forM_ mnext $ \(gpioGrpAct', gpioPinAct') -> do
                call_ GPIO.writeBit gpioGrpAct' gpioPinAct' GPIO.bit_SET
        ]
      )

area_btn_presses :: MemArea ('Stored Uint8)
area_btn_presses = area btn_presses (Just $ ival 0)

area_btn_current_state :: MemArea (Array BtnCount ('Stored IBool))
area_btn_current_state = area btn_current_state (Just $ iarray $ replicate btnCount (izero))

area_btn_debounce_release :: MemArea (Array BtnCount ('Stored Uint8))
area_btn_debounce_release = area btn_debounce_release Nothing

area_btn_ignore :: MemArea (Array BtnCount ('Stored Uint8))
area_btn_ignore = area btn_ignore (Just $ iarray $ replicate btnCount (izero))

btn_presses = "btn_presses"
btn_ignore = "btn_ignore"
btn_current_state = "btn_current_state"
btn_debounce_release = "btn_debounce_release"


enumerateOf t = flip evalState 0 . mapMOf t (\a -> do { j <- get; modify succ; pure (j, a) })

imatrix = enumerateOf (traverse . _2 . traverse) matrix

type BtnCount = 5
btnCount = sum . fmap (length . snd) $ matrix
rowCount = length matrix
matrix =
  [ ( b13, [ a8, a9, a10 ] )
  , ( b14, [     a9, a10 ] )
  -- , ( b14, [     a9, a10 ] )
  ]

periodTicks = length matrix_schedule
oneTimeMatrixScanPeriodMicroseconds = 500
tickPeriodMicroseconds :: Double
tickPeriodMicroseconds = oneTimeMatrixScanPeriodMicroseconds / fromIntegral periodTicks


main' :: Def ('[] ':-> ())
main' = proc "main" $ body $ do
  call_ usb_ionic_prepare

  -- enable clock for GPIO ports
  call_ RCC._APB2PeriphClockCmd (
         RCC._APB2Periph_GPIOB
      .| RCC._APB2Periph_GPIOC
      .| RCC._APB2Periph_GPIOA
      ) enable

  -- initialize GPIO structure
  s <- local (istruct [])
  call_ GPIO.structInit s
  gpioInit s gpioC GPIO.pin_13 GPIO.mode_Out_PP GPIO.speed_2MHz

  gpioInit s gpioB GPIO.pin_13 GPIO.mode_Out_PP GPIO.speed_2MHz

  gpioInit s gpioA GPIO.pin_8 GPIO.mode_IPD GPIO.speed_2MHz
  gpioInit s gpioA GPIO.pin_9 GPIO.mode_IPD GPIO.speed_2MHz

  -- gpioInit s gpioB GPIO.pin_9 GPIO.mode_IN_FLOATING GPIO.speed_10MHz

  -- set up a timer
  call_ sysTick_Config (systemCoreClock ./ (fromIntegral . round $ 1_000_000 / tickPeriodMicroseconds))

  call_ handle_usb_loop

  retVoid


handle_usb_loop :: Def ('[] ':-> ())
handle_usb_loop = importProc "handle_usb_loop" "usb_main.h"

usb_ionic_prepare :: Def ('[] ':-> ())
usb_ionic_prepare = importProc "usb_ionic_prepare" "usb_main.h"


handle_press :: Ix BtnCount -> Ivory eff ()
handle_press j = do
    modifyVar area_btn_presses (+1)
    pure ()

-- handle_release :: Int -> Ivory eff ()
-- handle_release j = do
--     pure ()

-- handle_press :: Def ('[ Ix BtnCount ] ':-> ())
-- handle_press = proc "handle_press" $ \j -> body $ do
--     retVoid

-- handle_release :: Def ('[ Ix BtnCount ] ':-> ())
-- handle_release = proc "handle_release" $ \j -> body $ do
--     retVoid

process_raw_btn :: Def ('[ Ix BtnCount, IBool ] ':-> ())
process_raw_btn = proc "process_raw_btn" $ \j b -> body $ do
  let j_ignore = addrOf area_btn_ignore ! j
  let j_current_state = addrOf area_btn_current_state ! j
  let j_debounce = addrOf area_btn_debounce_release ! j
  ignore <- deref j_ignore
  ifte_ (ignore >? 0)
    (do
        store j_ignore (ignore - 1)
    )
    (do
        current <- deref j_current_state
        ifte_ current
          (do
              ifte_ b
                (do
                    store j_debounce debounceRelease
                )
                (do
                    -- Если текущее состояние -- нажата, то отпускание
                    -- засчитаем только через ignoreAfterPress после нажатия
                    -- и debounceRelease стабильного отпускания.
                    debounce <- deref j_debounce
                    ifte_ (debounce >? 0)
                      (do
                          store j_debounce (debounce - 1)
                      )
                      (do
                          store j_current_state false
                          store j_ignore ignoreAfterRelease
                          -- call_ handle_release j
                          -- handle_release j
                      )
                )
          )
          (do
              ifte_ b
                (do
                    -- Если текущее состоянине - отжата, то засчитываем нажатие
                    -- сразу (но только если после отпускания прошло ignoreAfterRelease)
                    store j_current_state true
                    store j_ignore ignoreAfterPress
                    store j_debounce debounceRelease
                    -- call_ handle_press j
                    handle_press j
                )
                (do
                    pure ()
                )
          )
    )
  retVoid

  where
    ignoreAfterPress = fromMs 40
    ignoreAfterRelease = fromMs 20
    debounceRelease = fromMs 10
    fromMs v = fromIntegral . round $ v *1000 / oneTimeMatrixScanPeriodMicroseconds

-- #ifdef USE_FULL_ASSERT
-- void assert_failed(uint8_t* file, uint32_t line)
-- {
--   while(1) {}
-- }
-- #endif
-- Is it possible to add preprocessor direcitves?
assert_failed :: Def ('[ Ref a (Stored Uint8), Uint32 ] ':-> ())
assert_failed = proc "assert_failed" $ \ file line -> body $ do
  forever $ pure ()


gpioInit s reg pin mode speed = do
  store (s ~> GPIO.gpio_Pin) pin
  store (s ~> GPIO.gpio_Mode) mode
  store (s ~> GPIO.gpio_Speed) speed
  call_ GPIO.init reg s


scheduleParams :: ScheduleParams
-- scheduleParams = pilotInfo & \PilotInfo {..} -> do
scheduleParams = do
  let
    sched_name             = "ionic_schedule"
    -- sched_pilotStep        = importProc "step" (pilotInfo_fileName <> ".h")
    -- sched_pilotTemperature = area (pp_btnsState pilotInfo_params) $ Just $ ival true
    sched_matrix_schedule  = matrix_schedule
    sched_period           = periodTicks
    sched_blink_on         = lightOn
    sched_blink_off        = lightOff
  ScheduleParams {..}


-- pilotInfo :: PilotInfo
-- pilotInfo = PilotInfo
--     { pilotInfo_params  = PilotParams
--         { pp_btnsState = "btns_state"
--         }
--     , pilotInfo_actions = PilotActions
--         { pa_blinkOn  = blinkOn'
--         , pa_blinkOff = blinkOff'
--         }
--     , pilotInfo_fileName = "pilot"
--     }



blinkOn' = "blinkon"
blinkOff' = "blinkoff"

blinkOn :: Def ('[] ':-> ())
blinkOn = proc blinkOn' $ body $ lightOn

-- lightOn :: IvoryAction ()
lightOn = do
    call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_RESET

blinkOff :: Def ('[] ':-> ())
blinkOff = proc blinkOff' $ body $ lightOff

-- lightOff :: IvoryAction ()
lightOff = do
    call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_SET
