{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

module Ivored.MainIvored where

-- import Data.Function
import Ivory.Language as IL
import Ivory.Language.Module

import Control.Monad
import Control.Monad.Writer.Strict (Writer, runWriter, tell)

import Control.Lens
import Control.Monad (forM, forM_, join)
import Control.Monad.State (evalState, get, modify)
import           Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC
import qualified Ivored.Inc.STM32F10x.USB as USB

import GHC.Types (Symbol)
import GHC.TypeNats

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

type CModule = Writer [ModuleDef]

runCModule :: CModule a -> (a, ModuleM ())
runCModule wr = do
    let (a, w) = runWriter wr
    (a, void $ mconcat w)

cdef :: CInclude a => a -> Writer [ModuleDef] a
cdef a = do
    tell . (:[]) $ inc a
    pure a

makeCModule :: (ScheduleParams, Module)
makeCModule = (scheduleParams, ) $ package "main" $ do
        inc RCC._APB2PeriphClockCmd
        inc RCC._APB2Periph_GPIOA
        inc RCC._APB2Periph_GPIOB
        inc RCC._APB2Periph_GPIOC
        inc enable
        inc GPIO.structInit
        inc (Proxy :: Proxy "GPIO_InitTypeDef_mock")
        inc GPIO.init
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

        cmodule

  where
        -- inc assert_failed

    (scheduleParams, cmodule) = runCModule $ mdo

        blinkOn <- cdef $ proc @('[] ':-> ()) "blinkon" $ body $ do
            lightOn

        blinkOff <- cdef $ proc @('[] ':-> ()) "blinkoff" $ body $ do
            lightOff

        ionicSchedule :: Def ('[] ':-> ()) <- cdef $
            importProc sched_name (sched_name <> ".h")

        _ :: Def ('[] ':-> ()) <- cdef $ proc "SysTick_Handler" $ body $ do
            call_ ionicSchedule

        area_btn_current_state :: MemArea (Array BtnCount ('Stored Uint32)) <- cdef $ area "btn_current_state" $
            Just $ iarray $ replicate btnCount (izero)

        process_raw_btn <- processRawBtn area_btn_current_state oneTimeMatrixScanPeriodMicroseconds

        bit_SET <- cdef GPIO.bit_SET
        bit_RESET <- cdef GPIO.bit_RESET
        writeBit :: Def ('[Ref a ('Struct "GPIO_TypeDef"), Uint16, BitAction] ':-> ()) <- cdef $ GPIO.writeBit
        -- bitChange :: BitAction -> Ref a ('Struct "GPIO_TypeDef") -> Uint16 -> Ivory eff ()
        let bitChange bitAction grp pin = call_ writeBit grp pin bitAction

        let
            matrix_schedule :: [Ivory eff ()]
            matrix_schedule =
                [ forM_ (fst <$> take 1 imatrix)
                    $ uncurry $ bitChange bit_SET
                ] <> join ( do
                  zip ((Just . fst <$> drop 1 imatrix) <> [Nothing]) imatrix
                    <&> \(mnext, ((gpioGrpAct, gpioPinAct), ipins)) ->
                      [ do
                          bs <- forM (snd <$> ipins) $ \(gpioGrpPas, gpioPinPas) -> do
                              fmap (/=?0) $ call GPIO.readInputDataBit gpioGrpPas gpioPinPas
                          bitChange bit_RESET gpioGrpAct gpioPinAct
                          forM_ (zip (fst <$> ipins) bs) $ \(j, b) -> call_ process_raw_btn (fromIntegral j) b
                          forM_ mnext $ uncurry $ bitChange bit_SET
                      ]
                    )
            periodTicks            = length matrix_schedule

            tickPeriodMicroseconds :: Double
            tickPeriodMicroseconds = oneTimeMatrixScanPeriodMicroseconds / fromIntegral periodTicks

            tickPeriodMilliseconds :: Double
            tickPeriodMilliseconds = tickPeriodMicroseconds / 1_000

            scheduleTicksFromMS = fromIntegral . round . (/ tickPeriodMilliseconds)

            sched_matrix_schedule  = matrix_schedule
            sched_period_matrix    = periodTicks
            sched_period_communicate_usb = scheduleTicksFromMS 1.618

        -- usb_ionic_prepare :: Def ('[] ':-> ()) <- cdef $ importProc "usb_ionic_prepare" "usb_main.h"

        pin_8 <- cdef GPIO.pin_8

        prepare_usb <- prepareUSB

        main' :: Def ('[] ':-> ()) <- cdef $ proc "main" $ body $ do
            noAlloc . noReturn $ prepare_usb

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
            call_ sysTick_Config' (systemCoreClock' ./ (fromIntegral . round $ 1_000_000 / tickPeriodMicroseconds))

            retVoid

        systemCoreClock' <- cdef systemCoreClock
        sysTick_Config' <- cdef sysTick_Config

        sched_communicate_usb <- communicateUSB

        pure $ ScheduleParams {..}
        where
            sched_name             = "ionic_schedule"
            sched_blink_on         = lightOn
            sched_blink_off        = lightOff

    oneTimeMatrixScanPeriodMicroseconds = 500

    enumerateOf t = flip evalState 0 . mapMOf t (\a -> do { j <- get; modify succ; pure (j, a) })

    imatrix = enumerateOf (traverse . _2 . traverse) matrix

    btnCount = sum . fmap (length . snd) $ matrix
    rowCount = length matrix
    matrix =
      [ ( b13, [ a8, a9, a10 ] )
      , ( b14, [     a9, a10 ] )
      -- , ( b14, [     a9, a10 ] )
      ]


type BtnCount = 5

processRawBtn :: forall bc. KnownNat bc =>
       MemArea ('Array bc ('Stored Uint32))
    -> Double
    -> CModule (Def ('[Ix bc, IBool] ':-> ()))
processRawBtn a_btn_current_state oneTimeMatrixScanPeriodMicroseconds = do
    let btnCount = fromIntegral $ natVal (Proxy :: Proxy bc)
    a_btn_debounce_release :: MemArea (Array bc ('Stored Uint8)) <- cdef $ area "btn_debounce_release" $
        Just $ iarray $ replicate btnCount (izero)
    -- a_btn_presses :: MemArea ('Stored Uint8) <- cdef $ area "btn_presses" $
    --     Just $ ival 0
    a_btn_ignore :: MemArea (Array bc ('Stored Uint8)) <- cdef $ area "btn_ignore" $
        Just $ iarray $ replicate btnCount (izero)
    cdef $ proc "process_raw_btn" $
        \j b -> body $
          do
              let
                  j_ignore = addrOf a_btn_ignore ! j
                  j_current_state = addrOf a_btn_current_state ! j
                  j_debounce = addrOf a_btn_debounce_release ! j

                  ignoreAfterPress = fromMs 40
                  ignoreAfterRelease = fromMs 20
                  debounceRelease = fromMs 10
                  fromMs v = fromIntegral . round $ v *1000 / oneTimeMatrixScanPeriodMicroseconds

              ignore <- deref j_ignore
              ifte_ (ignore >? 0)
                (do
                    store j_ignore (ignore - 1)
                )
                (do
                    current <- deref j_current_state
                    ifte_ (current >? 0)
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
                                      store j_current_state 0
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
                                store j_current_state $ (current <? maxBound) ? (current+1, current)
                                store j_ignore ignoreAfterPress
                                store j_debounce debounceRelease
                                -- call_ handle_press j
                                -- handle_press a_btn_presses j
                            )
                            (do
                                pure ()
                            )
                      )
                )
              retVoid



handle_press :: (IvoryStore a, Num a) => MemArea ('Stored a) -> Ix btnCount -> Ivory eff ()
handle_press area_v j = do
    modifyVar area_v (+1)
    pure ()

-- #ifdef USE_FULL_ASSERT
-- void assert_failed(uint8_t* file, uint32_t line)
-- {
--   while(1) {}
-- }
-- #endif
-- Is it possible to add preprocessor direcitves?
assert_failed :: Def ('[ Ref a (Stored Uint8), Uint32 ] ':-> ())
assert_failed = proc "assert_failed" $ \ file line -> body $ do
  IL.forever $ pure ()


gpioInit s reg pin mode speed = do
  store (s ~> GPIO.gpio_Pin) pin
  store (s ~> GPIO.gpio_Mode) mode
  store (s ~> GPIO.gpio_Speed) speed
  call_ GPIO.init reg s


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



-- lightOn :: IvoryAction ()
lightOn = do
    call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_RESET

-- lightOff :: IvoryAction ()
lightOff = do
    call_ GPIO.writeBit gpioC GPIO.pin_13 GPIO.bit_SET

prepareUSB :: CModule (Ivory NoEffects ())
prepareUSB = do
    set_system            <- cdef $ USB.setSystem
    set_sys_clock_to72    <- cdef $ USB.setSysClockTo72
    set_usb_clock         <- cdef $ USB.setUSBClock
    usb_interrupts_config <- cdef $ USB.usbInterruptsConfig
    usb_init              <- cdef $ USB.usbInit
    pure $ do
        call_ set_system
        call_ set_sys_clock_to72
        call_ set_usb_clock
        call_ usb_interrupts_config
        call_ usb_init

communicateUSB :: CModule (Ivory NoEffects ())
communicateUSB = do
    prev_xfer_complete      <- cdef $ USB.prevXferComplete
    b_device_state          <- cdef $ USB.bDeviceState
    device_state_configured <- cdef $ USB.deviceStateConfigured

    -- keyboard_send_6key_state <- cdef $ keyboard_send_6key_state
    pure $ do
        ift_ (b_device_state ==? device_state_configured) $ do
            is_prev_xfer_complete <- deref $ addrOf prev_xfer_complete
            ift_ (is_prev_xfer_complete >? 0) $ do

                -- keyboard_send_6key_state

                lightOn


--   {

--     if (bDeviceState == CONFIGURED)
--     {
--       if (PrevXferComplete)
--       {

--         // uint8_t buf[9] = {2,0,0,0,0,0,0,0,0};

--         // buf <- get next portion to send
--         // press_key(ch);  // press_key_mod(k, 0)
--         keyboard_send_6key_state(buf);

--           // KEYBOARD_SEND_word("132 ");
--           // MOUSE_move(1,-1);
--           //RHIDCheckState();
--       }

-- //          // If received symbol '1' then LED turn on, else LED turn off
-- //          if (Receive_Buffer[0]=='1') {
-- //              GPIO_ResetBits(GPIOC, GPIO_Pin_13);
-- //          }

--     }
--   }
-- }

