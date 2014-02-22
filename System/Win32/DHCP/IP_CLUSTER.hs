module System.Win32.DHCP.IP_CLUSTER
    ( IP_CLUSTER (..)
    ) where

import Control.Applicative
import Foreign

import System.Win32.Types

import Data.Ip

-- typedef struct _DHCP_IP_CLUSTER {
--   DHCP_IP_ADDRESS ClusterAddress;
--   DWORD           ClusterMask;
-- } DHCP_IP_CLUSTER, *LPDHCP_IP_CLUSTER;
data IP_CLUSTER = IP_CLUSTER !Ip !DWORD

instance Storable IP_CLUSTER where
  sizeOf _ = 8
  alignment _ = 1
  peek ptr = IP_CLUSTER <$> (peek . castPtr $ ptr)
      <*> (castPtr ptr `peekByteOff` 4)
  poke ptr (IP_CLUSTER ip cmask) = do
      poke (castPtr ptr) ip
      pokeByteOff (castPtr ptr) 4 cmask
