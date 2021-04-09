module Ivored.CModule where

import Control.Monad
import Control.Monad.Writer.Strict (Writer, runWriter, tell)
import GHC.Types (Symbol)

import Ivory.Language as IL
import Ivory.Language.Module

import           Ivored.Inc.STM32F10x.GPIO as GPIO
import qualified Ivored.Inc.STM32F10x.RCC as RCC
import qualified Ivored.Inc.STM32F10x.USB as USB


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
