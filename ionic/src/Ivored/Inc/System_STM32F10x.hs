module Ivored.Inc.System_STM32F10x where

import Ivory.Language
-- import Ivory.Compile.C.CmdlineFrontend


-- extern uint32_t SystemCoreClock;          /*!< System Clock Frequency (Core Clock) */
systemCoreClock :: Uint32
systemCoreClock = extern "SystemCoreClock" "system_stm32f10x.h"
