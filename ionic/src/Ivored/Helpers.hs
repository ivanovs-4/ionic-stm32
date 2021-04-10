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

caseValueBounded :: forall a b.
    ( IvoryEq a, IvoryOrd a, Num a, Integral b
    ) => a -> [(b, Ivory NoEffects ())] -> Ivory NoEffects () -> Ivory NoEffects ()
caseValueBounded a bs defb =
    if (null bs)
        then defb
        else do
          ifte_ ((a >=? fromIntegral a_min) .&& (a<=? fromIntegral a_max))
              (foldr one (pure ()) bs)
              defb
  where
    as = fst <$> bs
    a_max = maximum as
    a_min = minimum as
    one (a', x) b = ifte_ (a ==? fromIntegral a') x b
