module Ivored.Helpers where

import Ivory.Language


modifyVar :: IvoryStore a => MemArea ('Stored a) -> (a -> a) -> Ivory eff ()
modifyVar v f =
    store addrv . f =<< deref addrv
  where
    addrv = addrOf v
