module Ivored.Inc.STM32F10x.GPIO where

import Ivory.Language
-- import Ivory.Compile.C.CmdlineFrontend

import Ivored.Inc.STM32F10x hiding (hfile, imProc, ext)


hfile = "stm32f10x_gpio.h"
imProc a = importProc a hfile
ext a = extern a hfile


-- typedef enum
-- {
--   GPIO_Speed_10MHz = 1,
--   GPIO_Speed_2MHz,
--   GPIO_Speed_50MHz
-- }GPIOSpeed_TypeDef;
speed_10MHz :: Uint8
speed_10MHz = ext "GPIO_Speed_10MHz"

speed_2MHz :: Uint8
speed_2MHz = ext "GPIO_Speed_2MHz"

speed_50MHz :: Uint8
speed_50MHz = ext "GPIO_Speed_50MHz"


-- typedef enum
-- { GPIO_Mode_AIN = 0x0,
--   GPIO_Mode_IN_FLOATING = 0x04,
--   GPIO_Mode_IPD = 0x28,
--   GPIO_Mode_IPU = 0x48,
--   GPIO_Mode_Out_OD = 0x14,
--   GPIO_Mode_Out_PP = 0x10,
--   GPIO_Mode_AF_OD = 0x1C,
--   GPIO_Mode_AF_PP = 0x18
-- }GPIOMode_TypeDef;

mode_AIN :: Uint8
mode_AIN = ext "GPIO_Mode_AIN"

mode_IN_FLOATING :: Uint8
mode_IN_FLOATING = ext "GPIO_Mode_IN_FLOATING"

mode_IPD :: Uint8
mode_IPD = ext "GPIO_Mode_IPD"

mode_IPU :: Uint8
mode_IPU = ext "GPIO_Mode_IPU"

mode_Out_OD :: Uint8
mode_Out_OD = ext "GPIO_Mode_Out_OD"

mode_Out_PP :: Uint8
mode_Out_PP = ext "GPIO_Mode_Out_PP"

mode_AF_OD :: Uint8
mode_AF_OD = ext "GPIO_Mode_AF_OD"

mode_AF_PP :: Uint8
mode_AF_PP = ext "GPIO_Mode_AF_PP"

type GPIOMode_TypeDef = Uint8



-- newtype TypeDef (sym :: Symbol) = TypeDef { getTypeDef :: AST.Expr }

-- typedef struct
-- {
--   uint16_t GPIO_Pin;             /*!< Specifies the GPIO pins to be configured.
--                                       This parameter can be any value of @ref GPIO_pins_define */
--   GPIOSpeed_TypeDef GPIO_Speed;  /*!< Specifies the speed for the selected pins.
--                                       This parameter can be a value of @ref GPIOSpeed_TypeDef */
--   GPIOMode_TypeDef GPIO_Mode;    /*!< Specifies the operating mode for the selected pins.
--                                       This parameter can be a value of @ref GPIOMode_TypeDef */
-- }GPIO_InitTypeDef_mock;


-- type GPIO_Pin = Uint16

[ivory|
    struct GPIO_InitTypeDef_mock
    {
      gpio_Pin :: Uint16
    ; gpio_Speed :: Uint8
    ; gpio_Mode :: Uint8
    }
|]
-- initStruct = Proxy :: Proxy "GPIO_InitTypeDef_mock"


-- typedef enum
-- { Bit_RESET = 0,
--   Bit_SET
-- }BitAction;
type BitAction = Uint8
bit_RESET = ext "Bit_RESET" :: BitAction
bit_SET   = ext "Bit_SET"   :: BitAction


pin_0 :: Uint16
pin_0 = ext "GPIO_Pin_0"

pin_1 :: Uint16
pin_1 = ext "GPIO_Pin_1"

pin_2 :: Uint16
pin_2 = ext "GPIO_Pin_2"

pin_3 :: Uint16
pin_3 = ext "GPIO_Pin_3"

pin_4 :: Uint16
pin_4 = ext "GPIO_Pin_4"

pin_5 :: Uint16
pin_5 = ext "GPIO_Pin_5"

pin_6 :: Uint16
pin_6 = ext "GPIO_Pin_6"

pin_7 :: Uint16
pin_7 = ext "GPIO_Pin_7"

pin_8 :: Uint16
pin_8 = ext "GPIO_Pin_8"

pin_9 :: Uint16
pin_9 = ext "GPIO_Pin_9"

pin_10 :: Uint16
pin_10 = ext "GPIO_Pin_10"

pin_11 :: Uint16
pin_11 = ext "GPIO_Pin_11"

pin_12 :: Uint16
pin_12 = ext "GPIO_Pin_12"

pin_13 :: Uint16
pin_13 = ext "GPIO_Pin_13"

pin_14 :: Uint16
pin_14 = ext "GPIO_Pin_14"

pin_15 :: Uint16
pin_15 = ext "GPIO_Pin_15"

pin_16 :: Uint16
pin_16 = ext "GPIO_Pin_16"

pin_All :: Uint16
pin_All = ext "GPIO_Pin_All"

a1 = (gpioA, pin_1)
a2 = (gpioA, pin_2)
a3 = (gpioA, pin_3)
a4 = (gpioA, pin_4)
a5 = (gpioA, pin_5)
a6 = (gpioA, pin_6)
a7 = (gpioA, pin_7)
a8 = (gpioA, pin_8)
a9 = (gpioA, pin_9)
a10 = (gpioA, pin_10)
a11 = (gpioA, pin_11)
a12 = (gpioA, pin_12)
a13 = (gpioA, pin_13)
a14 = (gpioA, pin_14)
a15 = (gpioA, pin_15)

b1 = (gpioB, pin_1)
b2 = (gpioB, pin_2)
b3 = (gpioB, pin_3)
b4 = (gpioB, pin_4)
b5 = (gpioB, pin_5)
b6 = (gpioB, pin_6)
b7 = (gpioB, pin_7)
b8 = (gpioB, pin_8)
b9 = (gpioB, pin_9)
b10 = (gpioB, pin_10)
b11 = (gpioB, pin_11)
b12 = (gpioB, pin_12)
b13 = (gpioB, pin_13)
b14 = (gpioB, pin_14)
b15 = (gpioB, pin_15)

c1 = (gpioC, pin_1)
c2 = (gpioC, pin_2)
c3 = (gpioC, pin_3)
c4 = (gpioC, pin_4)
c5 = (gpioC, pin_5)
c6 = (gpioC, pin_6)
c7 = (gpioC, pin_7)
c8 = (gpioC, pin_8)
c9 = (gpioC, pin_9)
c10 = (gpioC, pin_10)
c11 = (gpioC, pin_11)
c12 = (gpioC, pin_12)
c13 = (gpioC, pin_13)
c14 = (gpioC, pin_14)
c15 = (gpioC, pin_15)



-- void GPIO_DeInit(GPIO_TypeDef* GPIOx);
-- void GPIO_AFIODeInit(void);

-- void GPIO_Init(GPIO_TypeDef* GPIOx, GPIO_InitTypeDef_mock* GPIO_InitTypeDef_mock);
init :: Def ('[ Ref a ('Struct "GPIO_TypeDef"), Ref a ('Struct "GPIO_InitTypeDef_mock")] ':-> ())
init = imProc "GPIO_Init"

-- void GPIO_StructInit(GPIO_InitTypeDef_mock* GPIO_InitTypeDef_mock);
structInit :: Def ('[ Ref a ('Struct "GPIO_InitTypeDef_mock")] ':-> ())
structInit = imProc "GPIO_StructInit"

-- uint8_t GPIO_ReadInputDataBit(GPIO_TypeDef* GPIOx, uint16_t GPIO_Pin);
readInputDataBit :: Def ('[ Ref a ('Struct "GPIO_TypeDef"), Uint16] ':-> BitAction)
readInputDataBit = imProc "GPIO_ReadInputDataBit"

-- uint16_t GPIO_ReadInputData(GPIO_TypeDef* GPIOx);
-- uint8_t GPIO_ReadOutputDataBit(GPIO_TypeDef* GPIOx, uint16_t GPIO_Pin);
-- uint16_t GPIO_ReadOutputData(GPIO_TypeDef* GPIOx);
-- void GPIO_SetBits(GPIO_TypeDef* GPIOx, uint16_t GPIO_Pin);
-- void GPIO_ResetBits(GPIO_TypeDef* GPIOx, uint16_t GPIO_Pin);

-- void GPIO_WriteBit(GPIO_TypeDef* GPIOx, uint16_t GPIO_Pin, BitAction BitVal);
writeBit :: Def ('[ Ref a ('Struct "GPIO_TypeDef"), Uint16, BitAction] ':-> ())
writeBit = imProc "GPIO_WriteBit"

-- void GPIO_Write(GPIO_TypeDef* GPIOx, uint16_t PortVal);
-- void GPIO_PinLockConfig(GPIO_TypeDef* GPIOx, uint16_t GPIO_Pin);
-- void GPIO_EventOutputConfig(uint8_t GPIO_PortSource, uint8_t GPIO_PinSource);
-- void GPIO_EventOutputCmd(FunctionalState NewState);
-- void GPIO_PinRemapConfig(uint32_t GPIO_Remap, FunctionalState NewState);
-- void GPIO_EXTILineConfig(uint8_t GPIO_PortSource, uint8_t GPIO_PinSource);
-- void GPIO_ETH_MediaInterfaceConfig(uint32_t GPIO_ETH_MediaInterface);

