module Ivored.Inc.STM32F10x.USB where

import Ivory.Language
-- import Ivory.Compile.C.CmdlineFrontend

import Ivored.Inc.STM32F10x hiding (hfile, imProc, ext)


hfile = "usb_pwr.h"
-- imProc a = importProc a hfile
ext a = extern a hfile

-- typedef enum _RESUME_STATE
-- {
--   RESUME_EXTERNAL,
--   RESUME_INTERNAL,
--   RESUME_LATER,
--   RESUME_WAIT,
--   RESUME_START,
--   RESUME_ON,
--   RESUME_OFF,
--   RESUME_ESOF
-- } RESUME_STATE;

-- typedef enum _DEVICE_STATE
-- {
--   UNCONNECTED,
--   ATTACHED,
--   POWERED,
--   SUSPENDED,
--   ADDRESSED,
--   CONFIGURED
-- } DEVICE_STATE;
deviceStateConfigured :: Uint32
deviceStateConfigured = ext "CONFIGURED"

bDeviceState :: Uint32
bDeviceState = extern "bDeviceState" hfile

prevXferComplete :: MemArea ('Stored Uint8)
prevXferComplete =  area "PrevXferComplete" $ Just $ ival 1

setSystem :: Def ('[] ':-> ())
setSystem = importProc "Set_System" "hw_config.h"

setSysClockTo72 :: Def ('[] ':-> ())
setSysClockTo72 = importProc "SetSysClockTo72" "usb_main.h"

setUSBClock :: Def ('[] ':-> ())
setUSBClock = importProc "Set_USBClock" "hw_config.h"

usbInterruptsConfig :: Def ('[] ':-> ())
usbInterruptsConfig = importProc "USB_Interrupts_Config" "hw_config.h"

usbInit :: Def ('[] ':-> ())
usbInit = importProc "USB_Init" "usb_init.h"
