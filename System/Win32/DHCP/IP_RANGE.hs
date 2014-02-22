module System.Win32.DHCP.IP_RANGE
    ( IP_RANGE (..)
    ) where

import Control.Applicative
import Foreign

import Data.Ip

data IP_RANGE = IP_RANGE !Ip !Ip

instance Storable IP_RANGE where
  sizeOf _ = 8
  alignment _ = 1
  peek ptr = IP_RANGE
      <$> (peek . castPtr) ptr
      <*> castPtr ptr `peekByteOff` 4
  poke ptr (IP_RANGE start end) = do
      pokeElemOff addrPtr 0 start
      pokeElemOff addrPtr 1 end
    where
      addrPtr = castPtr ptr :: Ptr Ip
