module System.Win32.DHCP.SUBNET_CLIENT_INFO_ARRAY_V4
  ( SUBNET_CLIENT_INFO_ARRAY_V4 (..)
  , clientInfoArray
  ) where

import System.Win32.DHCP.Client
import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.LengthBuffer

-- typedef struct _DHCP_CLIENT_INFO_ARRAY_V4 {
--   DWORD                 NumElements;
--   LPDHCP_CLIENT_INFO_V4 *Clients;
-- } DHCP_CLIENT_INFO_ARRAY_V4, *LPDHCP_CLIENT_INFO_ARRAY_V4;
newtype SUBNET_CLIENT_INFO_ARRAY_V4
    = SUBNET_CLIENT_INFO_ARRAY_V4 (LengthBuffer Client)

unwrap :: SUBNET_CLIENT_INFO_ARRAY_V4 -> LengthBuffer Client
unwrap (SUBNET_CLIENT_INFO_ARRAY_V4 ia) = ia

clientInfoArray :: DhcpStructure SUBNET_CLIENT_INFO_ARRAY_V4
clientInfoArray = newtypeDhcpStructure SUBNET_CLIENT_INFO_ARRAY_V4 unwrap
    $ lengthBuffer (ptrDhcpArray clientInfo)
