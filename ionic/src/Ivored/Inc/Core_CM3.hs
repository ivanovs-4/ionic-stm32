module Ivored.Inc.Core_CM3 where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend


-- static __INLINE uint32_t SysTick_Config(uint32_t ticks)
sysTick_Config :: Def ('[Uint32] ':-> Uint32)
sysTick_Config = importProc "SysTick_Config" "core_cm3.h"
