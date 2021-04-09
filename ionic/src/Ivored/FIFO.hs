module Ivored.FIFO where

import Ivory.Language as IL
import GHC.TypeNats

import Ivored.CModule
import Ivored.Helpers as H


data FIFO n a = FIFO
  { fifo_isEmpty :: Ivory NoEffects IBool
  , fifo_isFull  :: Ivory NoEffects IBool
  , fifo_put     :: MemArea a -> Ivory NoEffects ()
  , fifo_head    :: Ivory NoEffects (Ref Global a)
  , fifo_putHead :: Ivory NoEffects ()
  , fifo_get     :: Ivory NoEffects (Ref Global a)
  }

ringFIFO :: forall n a.
    ( KnownNat n
    , IvoryArea a , IvoryZero a
    ) => String -> CModule (FIFO n a)
ringFIFO name = do

    ring :: MemArea (Array n a) <- cdef $ area (name <> "_ring") Nothing
    wIx :: MemArea (Stored (Ix n)) <- cdef $ area (name <> "_wix") $ Just $ ival 0
    rIx :: MemArea (Stored (Ix n)) <- cdef $ area (name <> "_rix") $ Just $ ival 0

    let
      -- fifo_isEmpty :: Ivory ref IBool
      fifo_isEmpty = do
          wIx' <- deref $ addrOf wIx
          rIx' <- deref $ addrOf rIx
          pure $ wIx' ==? rIx'

    let
      -- fifo_isFull :: Ivory ref IBool
      fifo_isFull = do
          wIx' <- deref $ addrOf wIx
          rIx' <- deref $ addrOf rIx
          pure $ (wIx' + 1) ==? rIx'

    let
      -- fifo_put :: MemArea (Stored a) -> Ivory ref ()
      fifo_put a = do
          wIx' <- deref $ addrOf wIx
          store (addrOf wIx) (wIx' + 1)
          refCopy (addrOf ring ! wIx') (addrOf a)

    let
      -- fifo_head :: Ivory ref ()
      fifo_head = do
          rIx' <- deref $ addrOf rIx
          pure $ addrOf ring ! rIx'

    let
      -- fifo_putHead :: Ivory ref ()
      fifo_putHead = do
          wIx' <- deref $ addrOf wIx
          store (addrOf wIx) (wIx' + 1)

    let
      -- fifo_get :: Ivory ref a
      fifo_get = do
          rIx' <- deref $ addrOf rIx
          store (addrOf rIx) (rIx' + 1)
          pure $ addrOf ring ! rIx'

    pure FIFO {..}

fifo_putSafe :: FIFO n a -> MemArea a -> Ivory NoEffects ()
fifo_putSafe FIFO {..} a = do
    isFull <- fifo_isFull
    ift_ (iNot isFull) $ do
        fifo_put a

fifo_putHeadSafe :: FIFO n a -> MemArea a -> Ivory NoEffects ()
fifo_putHeadSafe FIFO {..} a = do
    isFull <- fifo_isFull
    ift_ (iNot isFull) $ do
        fifo_putHead

fifo_withHead :: FIFO n a -> (Ref 'Global a -> Ivory NoEffects ()) -> Ivory NoEffects ()
fifo_withHead FIFO {..} f = do
    isFull <- fifo_isFull
    ift_ (iNot isFull) $ do
        head <- fifo_head
        f head
        fifo_putHead
