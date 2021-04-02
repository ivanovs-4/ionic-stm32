{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivored.MainIvored where

import Data.Function
import Ivory.Language

import Control.Lens
import Control.Monad (forM, forM_, join)
import Control.Monad.State (evalState, get, modify)
import           Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC

import Ivored.Helpers as H
import Ivored.Inc.STM32F10x
import Pilot
import Schedule

[ivory|
    struct btn_state
      { on_off    :: Stored IBool
      ; lasts_on  :: Stored Uint16
      ; lasts_off :: Stored Uint16
      }
    |]

cmodule :: ScheduleParams -> Module
cmodule ScheduleParams {..} = package "main" $ do
  incl RCC._APB2PeriphClockCmd
  inclSym RCC._APB2Periph_GPIOA
  inclSym RCC._APB2Periph_GPIOB
  inclSym RCC._APB2Periph_GPIOC
  inclSym enable
  incl GPIO.structInit
  defStruct (Proxy :: Proxy "GPIO_InitTypeDef_mock")
  incl GPIO.init
  inclSym GPIO.bit_SET
  inclSym GPIO.bit_RESET
  inclSym GPIO.pin_8
  inclSym GPIO.pin_9
  inclSym GPIO.pin_13
  inclSym GPIO.mode_IPD
  inclSym GPIO.mode_Out_PP
  inclSym GPIO.mode_IN_FLOATING
  inclSym GPIO.speed_2MHz
  inclSym GPIO.speed_10MHz
  inclSym GPIO.speed_50MHz
  incl GPIO.writeBit
  incl GPIO.readInputDataBit
  inclSym gpioA
  inclSym gpioB
  inclSym gpioC
  incl sysTick_Config
  inclSym systemCoreClock
  --
  incl main'
  incl sysTick_Handler
  incl ionicSchedule
  incl sched_pilotStep
  defStruct (Proxy :: Proxy "btn_state")

  -- istruct [field0 .= 3; field1 .= 4]

  incl blinkOn
  incl blinkOff
  defMemArea sched_pilotTemperature

  -- defMemArea ((area "areaname" $ Just $ ival true) :: MemArea ('Stored IBool))
  -- defMemArea ((area "areaname" Nothing) :: MemArea (Array 4 ('Stored IBool)))

  incl initButtons
  incl checkButtons
  defMemArea rowNum

  defMemArea area_raw_btn_scan
  incl process_raw_btn

  --

  -- incl assert_failed

  incl usb_ionic_prepare
  incl handle_usb_loop

  where
      ionicSchedule :: Def ('[] ':-> ())
      ionicSchedule = importProc sched_name (sched_name <> ".h")

      sysTick_Handler :: Def ('[] ':-> ())
      sysTick_Handler = proc "SysTick_Handler" $ body $ do
          call_ ionicSchedule



area_raw_btn_scan :: MemArea (Array 7 ('Stored IBool))
area_raw_btn_scan = area raw_btn_scan (Just $ iarray $ replicate btnCount (izero))

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
                call GPIO.readInputDataBit gpioGrpPas gpioPinPas
            call_ GPIO.writeBit gpioGrpAct gpioPinAct GPIO.bit_RESET
            forM_ (zip (fst <$> ipins) bs) $ \(j, b) -> do
                store (addrOf area_raw_btn_scan ! (fromIntegral j)) (b/=?0)
        ] <>
        [ do
            forM_ mnext $ \(gpioGrpAct', gpioPinAct') -> do
                call_ GPIO.writeBit gpioGrpAct' gpioPinAct' GPIO.bit_SET
            forM_ (fst <$> ipins) $ call_ process_raw_btn . fromIntegral
        ]
      )


raw_btn_scan = "raw_btn_scan"

enumerateOf t = flip evalState 0 . mapMOf t (\a -> do { j <- get; modify succ; pure (j, a) })

imatrix = enumerateOf (traverse . _2 . traverse) matrix

btnCount = sum . fmap (length . snd) $ matrix
matrix =
  [ ( b13, [ a8, a9, a10 ] )
  , ( b14, [     a9, a10 ] )
  ]




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

  call_ initButtons
  -- gpioInit s gpioB GPIO.pin_9 GPIO.mode_IN_FLOATING GPIO.speed_10MHz

  -- set up a timer
  call_ sysTick_Config (systemCoreClock ./ 7_200)  -- Сработает 10_000 раз в секунду

  call_ handle_usb_loop

  retVoid


handle_usb_loop :: Def ('[] ':-> ())
handle_usb_loop = importProc "handle_usb_loop" "usb_main.h"

usb_ionic_prepare :: Def ('[] ':-> ())
usb_ionic_prepare = importProc "usb_ionic_prepare" "usb_main.h"


process_raw_btn :: Def ('[ Uint8 ] ':-> ())
process_raw_btn = proc "process_raw_btn" $ \j -> body $ do
  retVoid

{-
  -- Строка установлена в предыдущем периоде и уже выполнено несколько измерений.
  1. Ещё раз замерил значения в (теперь уже в текущей) строке.
  1. Переключил на следующую строку (пока дальше идёт обсчёт, напряжения устанавливаются).
  1. Для каждого элемента измеренной строки обсчитал измерения с голосованием
        и учётом предыдущего измерения. Смена состояния происходит только если все
        проголосовали одинаково. И только если после предыдущей смены состояния
        прошло не меньше 48мс.
  1. Обменялся с отдельной частью данными о состоянии кнопок.
  1. Проинтерпретировал нажатия на обеих частях с учётом имеющегося состояния
        модификаторов и режимов (мод).
        Результаты отправил scan-code в кольцевой буфер usb.
  1. Замерил значения в (следующей) строке.
  1. Для каждого измеренного элемента строки вычислил новое значение подсветки,
        прописал на вход dma и отправил на отдельную часть.
  1. Ещё раз замерил значения в (следующей) строке.
  --
-}


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
scheduleParams = pilotInfo & \PilotInfo {..} -> do
  let
    sched_name             = "ionic_schedule"
    sched_pilotStep        = importProc "step" (pilotInfo_fileName <> ".h")
    sched_pilotTemperature = area (pp_btnsState pilotInfo_params) $ Just $ ival true
    sched_checkButtons     = call_ checkButtons
    sched_matrix_schedule  = matrix_schedule
  ScheduleParams {..}


pilotInfo :: PilotInfo
pilotInfo = PilotInfo
    { pilotInfo_params  = PilotParams
        { pp_btnsState = "btns_state"
        }
    , pilotInfo_actions = PilotActions
        { pa_blinkOn  = blinkOn'
        , pa_blinkOff = blinkOff'
        }
    , pilotInfo_fileName = "pilot"
    }


initButtons :: Def ('[] ':-> ())
initButtons = proc "init_buttons" $ body $ do
    lightOff
    retVoid

checkButtons :: Def ('[] ':-> ())
checkButtons = proc "check_buttons" $ body $ do

    H.modifyVar rowNum ((.% 2) . (+1))

    -- call_ GPIO.writeBit gpioB GPIO.pin_13 GPIO.bit_SET
    -- b1 <- call GPIO.readInputDataBit gpioA GPIO.pin_8
    -- b2 <- call GPIO.readInputDataBit gpioA GPIO.pin_9
    -- call_ GPIO.writeBit gpioB GPIO.pin_13 GPIO.bit_RESET
    -- ifte_ (b1 /=? 0)
    --   (lightOn)
    --   (pure ())
    -- ifte_ (b2 /=? 0)
    --   (lightOff)
    --   (pure ())

    retVoid

rowNum :: MemArea ('Stored Uint8)
rowNum = area "row_num" $ Just $ ival 0



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
