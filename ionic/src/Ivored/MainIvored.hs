{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivored.MainIvored where

-- import Data.Function
import Ivory.Language as IL
import Ivory.Language.Module

import Control.Arrow
import Control.Monad

import Control.Lens hiding ((.=))
import Control.Monad (forM, forM_, join)
import Control.Monad.State (evalState, get, modify)
import           Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC
import qualified Ivored.Inc.STM32F10x.USB as USB

import Data.Functor
import Data.Text (Text)
import GHC.TypeNats

import Ivored.CModule
import Ivored.Helpers as H
import Ivored.FIFO
import Ivored.Keycodes
import Ivored.Inc.STM32F10x
-- import Pilot
import Schedule

--  * Keyboard buffer:
--  * buf[0]: MOD
--  * buf[1]: reserved
--  * buf[2]..buf[7] - keycodes 1..6

[ivory|
    struct Usb6keyStateMessage
      { endpoint  :: Stored Uint8
      ; keybuffer :: Array 9 (Stored Uint8)
      }
    |]

[ivory|
    struct BtnEvent
      { btnEvent_button :: Stored Uint8
      ; btnEvent_press  :: Stored IBool
      }
    |]

[ivory|
    struct KeyEvent
      { keyEvent_keycode :: Stored Uint8
      ; keyEvent_press   :: Stored IBool
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
        inc (Proxy :: Proxy "BtnEvent")
        inc (Proxy :: Proxy "KeyEvent")
        inc GPIO.init
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
        btn_events :: FIFO BtnEventsFIFOSize ('Struct "BtnEvent") <- ringFIFO "btn_events"
        process_raw_btn <- processRawBtn
            oneTimeMatrixScanPeriodMicroseconds
            area_btn_current_state
            btn_events

        key_events :: FIFO KeyEventsFIFOSize ('Struct "KeyEvent") <- ringFIFO "key_events"

        interpretate_btn_events <- interpretateBtnEvents btn_events key_events

        bit_SET <- cdef GPIO.bit_SET
        bit_RESET <- cdef GPIO.bit_RESET
        writeBit :: Def ('[Ref a ('Struct "GPIO_TypeDef"), Uint16, BitAction] ':-> ()) <- cdef $ GPIO.writeBit
        -- bitChange :: BitAction -> Ref a ('Struct "GPIO_TypeDef") -> Uint16 -> Ivory eff ()
        let bitChange bitAction grp pin = call_ writeBit grp pin bitAction

        let
            matrix_schedule :: [Ivory NoEffects ()]
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
                          forM_ (zip (fst <$> ipins) bs) $ \(j, b) -> do
                              call_ process_raw_btn (fromIntegral j) b
                          forM_ mnext $ uncurry $ bitChange bit_SET
                      ]
                    )
                <> [
                    interpretate_btn_events
                ]
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
        pin_9 <- cdef GPIO.pin_9
        pin_10 <- cdef GPIO.pin_10

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

            gpioInit s gpioA pin_8 GPIO.mode_IPD GPIO.speed_2MHz
            gpioInit s gpioA pin_9 GPIO.mode_IPD GPIO.speed_2MHz
            gpioInit s gpioA pin_10 GPIO.mode_IPD GPIO.speed_2MHz

            -- gpioInit s gpioB GPIO.pin_9 GPIO.mode_IN_FLOATING GPIO.speed_10MHz

            -- set up a timer
            call_ sysTick_Config' (systemCoreClock' ./ (fromIntegral . round $ 1_000_000 / tickPeriodMicroseconds))

            IL.forever $ pure ()


        systemCoreClock' <- cdef systemCoreClock
        sysTick_Config' <- cdef sysTick_Config

        sched_communicate_usb <- communicateUSB key_events

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

type KeyCount = 255

type BtnEventsFIFOSize = 12
type KeyEventsFIFOSize = 40

type BtnCount = 5

processRawBtn :: forall bc evsn. (KnownNat bc, KnownNat evsn) =>
       Double
    -> MemArea ('Array bc ('Stored Uint32))
    -> FIFO evsn ('Struct "BtnEvent")
    -> CModule (Def ('[Ix bc, IBool] ':-> ()))
processRawBtn
    oneTimeMatrixScanPeriodMicroseconds
    a_btn_current_state
    btn_events
  = do
    a_btn_debounce_release :: MemArea (Array bc ('Stored Uint8)) <- cdef $ area "btn_debounce_release" $
        Just $ iarray $ replicate (numVal (Proxy @bc)) (izero)
    a_btn_ignore :: MemArea (Array bc ('Stored Uint8)) <- cdef $ area "btn_ignore" $
        Just $ iarray $ replicate (numVal (Proxy @bc)) (izero)

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

                                      -- handle_release j
                                      noAlloc . noReturn $
                                          fifo_withHead btn_events $ \h -> do
                                              store (h ~> btnEvent_button) (safeCast j)
                                              store (h ~> btnEvent_press) false

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

                                -- handle_press j
                                noAlloc . noReturn $
                                    fifo_withHead btn_events $ \h -> do
                                        store (h ~> btnEvent_button) (safeCast j)
                                        store (h ~> btnEvent_press) true

                            )
                            (do
                                pure ()
                            )
                      )
                )
              retVoid



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

type EndpointsParallelCount = 1

type KeybufArraySize = 9
keybufArraySize :: Num n => n
keybufArraySize = fromIntegral $ numVal (Proxy @KeybufArraySize)

communicateUSB ::
      FIFO KeyEventsFIFOSize ('Struct "KeyEvent")
   -> CModule (Ivory NoEffects ())
communicateUSB key_events = do
    prev_xfer_complete      <- cdef $ USB.prevXferComplete
    b_device_state          <- cdef $ USB.bDeviceState
    device_state_configured <- cdef $ USB.deviceStateConfigured

    -- Will be used for the nkro hopefully
    parallel_keybufs :: MemArea (Array EndpointsParallelCount ('Struct "Usb6keyStateMessage")) <- cdef $ area "parallel_keybufs" $
        Just $ iarray $ replicate (numVal (Proxy @EndpointsParallelCount))
            $ istruct [ endpoint .= ival ep1, keybuffer .= iarray (ival 2 : replicate (pred keybufArraySize) (ival 0)) ]
    let
        current_key_buf :: Ref Global ('Struct "Usb6keyStateMessage")
        current_key_buf = addrOf parallel_keybufs ! 0

    a_usb_silence_ticks :: MemArea ('Stored Uint8) <- cdef $ area "a_usb_silence_ticks" $
        Just $ ival 0

    usb_send <- usbSend prev_xfer_complete current_key_buf

    pure $ do
        ift_ (b_device_state ==? device_state_configured) $ do
            is_prev_xfer_complete <- deref $ addrOf prev_xfer_complete
            ift_ (is_prev_xfer_complete >? 0) $ do

                isKeyEventsEmpty <- fifo_isEmpty key_events
                silenceTicks <- deref $ addrOf a_usb_silence_ticks

                ifte_ (isKeyEventsEmpty .&& (silenceTicks <? 100))
                    ( do
                        modifyVar a_usb_silence_ticks (+1)
                    )
                    ( do
                        ift_ (iNot isKeyEventsEmpty) $ do
                            ev <- fifo_get key_events
                            keypress <- deref $ ev ~> keyEvent_press
                            keycode <- deref $ ev ~> keyEvent_keycode
                            caseValue keycode
                              ( [ (key_LEFTCTRL   , key_MOD_LCTRL  )
                                , (key_LEFTSHIFT  , key_MOD_LSHIFT )
                                , (key_LEFTALT    , key_MOD_LALT   )
                                , (key_LEFTMETA   , key_MOD_LMETA  )
                                , (key_RIGHTCTRL  , key_MOD_RCTRL  )
                                , (key_RIGHTSHIFT , key_MOD_RSHIFT )
                                , (key_RIGHTALT   , key_MOD_RALT   )
                                , (key_RIGHTMETA  , key_MOD_RMETA  )
                                ] <&> first fromIntegral
                                    . second (\bit -> do
                                              modifyRef ((current_key_buf ~> keybuffer) ! 1) $
                                                  \mod -> keypress ? (mod .| bit, mod .& (iComplement bit))
                                              )
                                    . second fromIntegral
                              )
                              $ do

                                    -- clear possible key at first
                                        -- Ivory has bug for such expression
                                        -- It generates
                                        -- `int32_t n_cse10 = (int32_t) ((int32_t) 6 % 7 - (int32_t) 1);`
                                        -- while should
                                        -- `int32_t n_cse10 = (int32_t) (((int32_t) 6 - (int32_t) 1) % 7);`
                                    IL.for (6 :: Ix 7) $ \j -> do
                                        let pos6 = (3+) . toIx . fromIx $ j
                                        val <- deref $ ((current_key_buf ~> keybuffer) ! pos6)
                                        ift_ (val ==? keycode) $ do
                                            store ((current_key_buf ~> keybuffer) ! pos6) 0

                                    ift_ keypress $ do
                                        IL.for (6 :: Ix 7) $ \j -> do
                                            let pos6 = (3+) . toIx . fromIx $ j
                                            val <- deref $ ((current_key_buf ~> keybuffer) ! pos6)
                                            ift_ (val ==? 0) $ do
                                                -- Use first empty slot
                                                store ((current_key_buf ~> keybuffer) ! pos6) (safeCast keycode)
                                                breakOut

                        usb_send
                        store (addrOf a_usb_silence_ticks) 0
                    )

usbSend prev_xfer_complete current_key_buf = do

    usb_sil_write :: Def ('[Uint8, Ref Global (Array KeybufArraySize (Stored Uint8)), Uint32] ':-> ()) <- cdef $
        importProc "USB_SIL_Write" "usb_sil.h"

    set_ep_tx_valid :: Def ('[Uint8] ':-> ()) <- cdef $
        importProc "SetEPTxValid" "usb_regs.h"

    endpoint_addresses :: MemArea (Array 8 (Stored Uint8)) <- cdef $ area "endpoint_addresses" $
        Just $ iarray $ ival <$> [ 0x80 , 0x81 , 0x82 , 0x83 , 0x84 , 0x85 , 0x86 , 0x87 ]

    pure $ do

        ep <- deref $ current_key_buf ~> endpoint
        ep_addr <- deref $ addrOf endpoint_addresses ! toIx ep
        buf <- pure $ current_key_buf ~> keybuffer

        -- USB_SIL_Write(EP1_IN, buf, 9);
        call_ usb_sil_write ep_addr buf keybufArraySize
        -- PrevXferComplete = 0;
        store (addrOf prev_xfer_complete) 0
        -- SetEPTxValid(ENDP1);
        call_ set_ep_tx_valid ep


interpretateBtnEvents ::
       FIFO n ('Struct "BtnEvent")
    -> FIFO k ('Struct "KeyEvent")
    -> CModule (Ivory NoEffects ())
interpretateBtnEvents btn_events key_events = do

    btn_to_key :: MemArea (Array BtnCount (Stored Uint8)) <- cdef $ area "btn_to_key" $
        Just $ iarray $ (ival . fromIntegral) <$>
          -- [ key_A , key_B , key_C , key_D , key_E
          -- , key_F , key_G , key_H , key_I , key_J
          [ key_K , key_LEFTSHIFT , key_M , key_N , key_O
          -- , key_K , key_L , key_M , key_N , key_O
          -- , key_P , key_Q , key_R , key_S , key_T , key_U , key_V , key_W , key_X , key_Y , key_Z
          ]

    pure $ do
        is_keys_has_place <- iNot <$> fifo_isFull key_events
        ift_ is_keys_has_place $ do
            is_btn_new <- iNot <$> fifo_isEmpty btn_events
            ift_ (is_btn_new) $ do
                btn_new <- fifo_get btn_events
                button <- deref $ btn_new ~> btnEvent_button
                press  <- deref $ btn_new ~> btnEvent_press
                keycode <- deref $ addrOf btn_to_key ! toIx button
                fifo_withHead key_events $ \h -> do
                    store (h ~> keyEvent_keycode) keycode
                    store (h ~> keyEvent_press) press
