module Ivored.Inc.STM32F10x.RCC where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend


hfile = "stm32f10x_rcc.h"
imProc a = importProc a hfile
ext a = extern a hfile


-- void RCC_APB2PeriphClockCmd(uint32_t RCC_APB2Periph, FunctionalState NewState);
_APB2PeriphClockCmd :: Def ('[Uint32, Uint8] ':-> ())
_APB2PeriphClockCmd  = imProc "RCC_APB2PeriphClockCmd"

_APB2Periph_GPIOA :: Uint32
_APB2Periph_GPIOA  = ext "RCC_APB2Periph_GPIOA"

_APB2Periph_GPIOB :: Uint32
_APB2Periph_GPIOB  = ext "RCC_APB2Periph_GPIOB"

_APB2Periph_GPIOC :: Uint32
_APB2Periph_GPIOC  = ext "RCC_APB2Periph_GPIOC"

_APB2Periph_GPIOD :: Uint32
_APB2Periph_GPIOD  = ext "RCC_APB2Periph_GPIOD"
