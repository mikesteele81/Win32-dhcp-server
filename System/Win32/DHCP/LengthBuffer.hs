{-# LANGUAGE RankNTypes #-}

module System.Win32.DHCP.LengthBuffer
  ( LengthBuffer (..)
  , lengthBuffer
  ) where

import Control.Applicative
import Foreign.Ptr
import Foreign.Storable
import System.Win32.Types (DWORD)

import System.Win32.DHCP.DhcpStructure

import Utils

-- |A LengthBuffer is a list of items which can be marshalled in and out
-- of memory with a `DhcpArray` instance. A C structure that can be used
-- with this looks something like the following:
-- 
-- typedef struct Structure {
--   DWORD         NumElements;
--   LPElementData Elements;
-- } Structure, *LPStructure;
data LengthBuffer a = LengthBuffer
    { lbLength :: !Int
    , buffer :: [a]
    }

lengthBuffer :: DhcpArray a -> DhcpStructure (LengthBuffer a)
lengthBuffer dict = DhcpStructure
    { peekDhcp = peekLb dict
    , freeDhcpChildren = freeLb dict
    , withDhcp' = withLb' dict
    , sizeDhcp = 8
    }

peekLb :: DhcpArray a -> Ptr (LengthBuffer a) -> IO (LengthBuffer a)
peekLb dict ptr = do
    len <- fromIntegral <$> peek (pNumElements ptr)
    pElements <- peek $ ppElements ptr
    LengthBuffer len <$> peekDhcpArray dict len pElements

freeLb :: DhcpArray a -> (forall x. Ptr x -> IO ())
    -> Ptr (LengthBuffer a) -> IO ()
freeLb dict freefunc ptr = do
    len <- fromIntegral <$> peek (pNumElements ptr)
    -- A LengthBuffer contain a pointer to the buffer; not the start of the
    -- buffer itself.
    freeDhcpArray dict freefunc len `scrubbing_` ppElements ptr

withLb' :: DhcpArray a -> LengthBuffer a -> Ptr (LengthBuffer a) -> IO r -> IO r
withLb' dict (LengthBuffer _ elems) ptr f =
    withDhcpArray dict elems $ \size pelems -> do
    pokeByteOff ptr 0 (fromIntegral size :: DWORD)
    poke (ppElements ptr) (castPtr pelems)
    f

pNumElements :: Ptr (LengthBuffer a) -> Ptr DWORD
pNumElements ptr = castPtr ptr

ppElements :: Ptr (LengthBuffer a) -> Ptr (Ptr a)
ppElements ptr = castPtr ptr `plusPtr` 4
