{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Ivored.MainIvored where

-- import Data.Function
import Ivory.Language as IL
import Ivory.Language.Module

import Control.Monad
import Control.Monad.Writer.Strict (Writer, runWriter, tell)

import Control.Lens hiding ((.=))
import Control.Monad (forM, forM_, join)
import Control.Monad.State (evalState, get, modify)
import           Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC
import qualified Ivored.Inc.STM32F10x.USB as USB

import GHC.Types (Symbol)
import GHC.TypeNats

import Ivored.Helpers as H
import Ivored.Keycodes
import Ivored.Inc.STM32F10x
-- import Pilot
import Schedule

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

--  * Keyboard buffer:
--  * buf[1]: MOD
--  * buf[2]: reserved
--  * buf[3]..buf[8] - keycodes 1..6
[ivory|
    struct Usb6keyStateMessage
      { endpoint  :: Stored Uint8
      ; keybuffer :: Array 9 (Stored Uint8)
      }
    |]


makeCModule :: (ScheduleParams, Module)
makeCModule = (scheduleParams, ) $ package "main" $ do
        inc RCC._APB2PeriphClockCmd
        inc RCC._APB2Periph_GPIOA
        inc RCC._APB2Periph_GPIOB
        inc RCC._APB2Periph_GPIOC
        inc enable
        inc GPIO.structInit
        inc (Proxy :: Proxy "GPIO_InitTypeDef_mock")
        inc (Proxy :: Proxy "Usb6keyStateMessage")
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

        ---------------- keys ----------
        a_keys :: MemArea (Array KeyCount ('Stored IBool)) <- cdef $ area "keys" $
            Just $ iarray $ replicate (numVal (Proxy @KeyCount)) (izero)

        process_raw_btn <- processRawBtn area_btn_current_state oneTimeMatrixScanPeriodMicroseconds a_keys

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

        sched_communicate_usb <- communicateUSB a_keys

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


numVal :: (KnownNat n, Num b) => proxy n -> b
numVal = fromIntegral . natVal

type KeyCount = 256

type BtnCount = 5

processRawBtn :: forall bc. KnownNat bc =>
       MemArea ('Array bc ('Stored Uint32))
    -> Double
    -> MemArea ('Array KeyCount ('Stored IBool))
    -> CModule (Def ('[Ix bc, IBool] ':-> ()))
processRawBtn a_btn_current_state oneTimeMatrixScanPeriodMicroseconds
    a_keys
  = do
    a_btn_debounce_release :: MemArea (Array bc ('Stored Uint8)) <- cdef $ area "btn_debounce_release" $
        Just $ iarray $ replicate (numVal (Proxy @bc)) (izero)
    a_btn_ignore :: MemArea (Array bc ('Stored Uint8)) <- cdef $ area "btn_ignore" $
        Just $ iarray $ replicate (numVal (Proxy @bc)) (izero)




    -- a_keys :: MemArea (Array KeyCount ('Stored IBool)) <- cdef $ area "keys" $
    --     Just $ iarray $ replicate (numVal (Proxy @KeyCount)) (izero)

    btn_to_key :: MemArea (Array bc (Stored Uint8)) <- cdef $ area "btn_to_key" $
        Just $ iarray $ (ival . fromIntegral) <$>
          [ key_A , key_B , key_C , key_D , key_E
          -- , key_F , key_G , key_H , key_I , key_J , key_K , key_L , key_M , key_N , key_O
          -- , key_P , key_Q , key_R , key_S , key_T , key_U , key_V , key_W , key_X , key_Y , key_Z
          ]

    let handle_press j = do
          k <- deref $ addrOf btn_to_key ! toIx j
          store (addrOf a_keys ! toIx k) true

    let handle_release j = do
          k <- deref $ addrOf btn_to_key ! toIx j
          store (addrOf a_keys ! toIx k) false



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
                                      handle_release j
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
                                handle_press j
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

ep1 = 1

type KeybufArraySize = 9
keybufArraySize = fromIntegral $ numVal (Proxy @KeybufArraySize)

communicateUSB ::
      MemArea (Array KeyCount ('Stored IBool))
   -> CModule (Ivory NoEffects ())
communicateUSB a_keys = do
    prev_xfer_complete      <- cdef $ USB.prevXferComplete
    b_device_state          <- cdef $ USB.bDeviceState
    device_state_configured <- cdef $ USB.deviceStateConfigured



    ring_key_buf :: MemArea (Array 3 ('Struct "Usb6keyStateMessage")) <- cdef $ area "ring_key_buf" $
        Just $ iarray $ replicate 3 $ istruct [ endpoint .= ival ep1, keybuffer .= iarray (ival 2 : replicate 8 (ival 0)) ]
    let
        current_key_buf :: Ref Global ('Struct "Usb6keyStateMessage")
        current_key_buf = addrOf ring_key_buf ! 2



    endpoint_addresses :: MemArea (Array 8 (Stored Uint8)) <- cdef $ area "endpoint_addresses" $
        Just $ iarray $ ival <$> [ 0x80 , 0x81 , 0x82 , 0x83 , 0x84 , 0x85 , 0x86 , 0x87 ]

    usb_sil_write :: Def ('[Uint8, Ref Global (Array KeybufArraySize (Stored Uint8)), Uint32] ':-> ()) <- cdef $
        importProc "USB_SIL_Write" "usb_sil.h"

    set_ep_tx_valid :: Def ('[Uint8] ':-> ()) <- cdef $
        importProc "SetEPTxValid" "usb_regs.h"

    -- a_usb_silence_ticks :: MemArea ('Stored Uint8) <- cdef $ area "a_usb_silence_ticks" $
    --     Just $ ival 0

    pure $ do

        IL.for (0 :: Ix 6) $ \j -> do
            let keyNum = j + fromIntegral key_A
            s <- deref (addrOf a_keys ! toIx keyNum)
            store ((current_key_buf ~> keybuffer) ! (toIx $ j+3)) (s ? (safeCast keyNum, 0))
            pure ()


            -- current_key_buf :: Ref Global ('Struct "Usb6keyStateMessage")
            -- current_key_buf = addrOf ring_key_buf ! 2

        ift_ (b_device_state ==? device_state_configured) $ do
            is_prev_xfer_complete <- deref $ addrOf prev_xfer_complete
            ift_ (is_prev_xfer_complete >? 0) $ do

                -- -- modifyVar a_usb_silence_ticks (+1)
                -- store (addrOf a_usb_silence_ticks) 0

                ep <- deref $ current_key_buf ~> endpoint
                ep_addr <- deref $ addrOf endpoint_addresses ! toIx ep
                buf <- pure $ current_key_buf ~> keybuffer

                -- USB_SIL_Write(EP1_IN, buf, 9);
                call_ usb_sil_write ep_addr buf keybufArraySize
                -- PrevXferComplete = 0;
                store (addrOf prev_xfer_complete) 0
                -- SetEPTxValid(ENDP1);
                call_ set_ep_tx_valid ep
