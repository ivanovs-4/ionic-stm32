module Ivored.Inc.STM32F10x
( module Ivored.Inc.STM32F10x
, module X
) where

import Ivory.Language
-- import Ivory.Compile.C.CmdlineFrontend

import Ivored.Inc.System_STM32F10x as X
import Ivored.Inc.Core_CM3 as X


ext a = extern a "stm32f10x.h"


enable :: Uint8
enable = ext "ENABLE"

-- functionalState ::
-- functionalState = ext "FunctionalState"


-- typedef struct
-- {
--   __IO uint32_t CRL;
--   __IO uint32_t CRH;
--   __IO uint32_t IDR;
--   __IO uint32_t ODR;
--   __IO uint32_t BSRR;
--   __IO uint32_t BRR;
--   __IO uint32_t LCKR;
-- } GPIO_TypeDef;

[ivory|
    struct GPIO_TypeDef
    {
      crl  :: Uint32
    ; crh  :: Uint32
    ; idr  :: Uint32
    ; odr  :: Uint32
    ; bsrr :: Uint32
    ; brr  :: Uint32
    ; lckr :: Uint32
    }
|]
-- gpio = Proxy :: Proxy "GPIO_TypeDef"

-- #define GPIOA               ((GPIO_TypeDef *) GPIOA_BASE)
-- #define GPIOB               ((GPIO_TypeDef *) GPIOB_BASE)
-- #define GPIOC               ((GPIO_TypeDef *) GPIOC_BASE)
-- #define GPIOD               ((GPIO_TypeDef *) GPIOD_BASE)
-- #define GPIOE               ((GPIO_TypeDef *) GPIOE_BASE)
-- #define GPIOF               ((GPIO_TypeDef *) GPIOF_BASE)
-- #define GPIOG               ((GPIO_TypeDef *) GPIOG_BASE)

gpioA :: Ref a ('Struct "GPIO_TypeDef")
gpioA = ext "GPIOA"

gpioB :: Ref a ('Struct "GPIO_TypeDef")
gpioB = ext "GPIOB"

gpioC :: Ref a ('Struct "GPIO_TypeDef")
gpioC = ext "GPIOC"

gpioD :: Ref a ('Struct "GPIO_TypeDef")
gpioD = ext "GPIOD"

gpioE :: Ref a ('Struct "GPIO_TypeDef")
gpioE = ext "GPIOE"

gpioF :: Ref a ('Struct "GPIO_TypeDef")
gpioF = ext "GPIOF"

gpioG :: Ref a ('Struct "GPIO_TypeDef")
gpioG = ext "GPIOG"
