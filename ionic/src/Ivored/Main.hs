{-# OPTIONS_GHC -fno-warn-orphans #-}

module Ivored.Main where

import Ivory.Language
import Ivory.Compile.C.CmdlineFrontend

import qualified Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC
import Ivored.Inc.STM32F10x


cmodule :: Module
cmodule = package "main" $ do
  incl RCC._APB2PeriphClockCmd
  inclSym RCC._APB2Periph_GPIOB
  inclSym RCC._APB2Periph_GPIOC
  inclSym enable
  incl GPIO.structInit
  defStruct (Proxy :: Proxy "GPIO_InitTypeDef_mock")
  incl GPIO.init
  inclSym GPIO.bit_SET
  inclSym GPIO.bit_RESET
  inclSym GPIO.pin_9
  inclSym GPIO.pin_13
  inclSym GPIO.mode_Out_PP
  inclSym GPIO.mode_IN_FLOATING
  inclSym GPIO.speed_10MHz
  inclSym GPIO.speed_2MHz
  inclSym gpioB
  inclSym gpioC
  incl sysTick_Config
  inclSym systemCoreClock
  --
  incl main'
  incl sysTick_Handler
  incl ionicSchedule
  -- incl assert_failed

main' :: Def ('[] ':-> ())
main' = proc "main" $ body $ do
  -- enable clock for GPIO ports
  call_ RCC._APB2PeriphClockCmd (
         RCC._APB2Periph_GPIOB
      .| RCC._APB2Periph_GPIOC
      ) enable

  -- initialize GPIO structure
  s <- local (istruct [])
  call_ GPIO.structInit s
  gpioInit s gpioC GPIO.pin_13 GPIO.mode_Out_PP GPIO.speed_2MHz
  gpioInit s gpioB GPIO.pin_9 GPIO.mode_IN_FLOATING GPIO.speed_10MHz


  -- set up a timer
  call_ sysTick_Config (systemCoreClock ./ 500)

  -- while (1)
  -- {
  --     GPIO_WriteBit(GPIOC, GPIO_Pin_13, Bit_SET);
  --     myDelay(144);
  --     GPIO_WriteBit(GPIOC, GPIO_Pin_13, Bit_RESET);
  --     myDelay(233);
  -- }

  retVoid


-- static volatile __IO uint32_t sysTickCount = 0;

-- void myDelay(uint32_t nTime)
-- {
--     sysTickCount = nTime;
--     while(sysTickCount != 0);
-- }

-- void SysTick_Handler(void)
-- {
--     if (sysTickCount != 0) {
--         sysTickCount--;
--     }
-- }

ionicSchedule :: Def ('[] ':-> ())
ionicSchedule = importProc "ionicSchedule" "ionicSchedule.h"

sysTick_Handler :: Def ('[] ':-> ())
sysTick_Handler = proc "SysTick_Handler" $ body $ do
  call_ ionicSchedule


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
