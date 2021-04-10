module Ivored.Helpers where

import Ivory.Language

import Data.Foldable


modifyVar :: IvoryStore a => MemArea ('Stored a) -> (a -> a) -> Ivory eff ()
modifyVar v f = modifyRef (addrOf v) f

modifyRef :: IvoryStore a => (Ref Global ('Stored a)) -> (a -> a) -> Ivory eff ()
modifyRef addrv f =
    store addrv . f =<< deref addrv

ift_ :: IBool -> Ivory eff a -> Ivory eff ()
ift_ c t = ifte_ c t (pure ())

caseValue :: IvoryEq a => a -> [(a, Ivory NoEffects ())] -> (Ivory NoEffects ()) -> Ivory NoEffects ()
caseValue a abs defb = foldr one defb abs
  where
    one (a', x) b = ifte_ (a ==? a') x b
