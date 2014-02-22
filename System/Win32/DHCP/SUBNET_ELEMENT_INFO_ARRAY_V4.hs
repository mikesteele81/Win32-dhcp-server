module System.Win32.DHCP.SUBNET_ELEMENT_INFO_ARRAY_V4
  ( SUBNET_ELEMENT_INFO_ARRAY_V4 (..)
  , infoArray
  ) where

import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.LengthBuffer
import System.Win32.DHCP.SUBNET_ELEMENT_DATA_V4

-- typedef struct _DHCP_SUBNET_ELEMENT_INFO_ARRAY_V4 {
--   DWORD                         NumElements;
--   LPDHCP_SUBNET_ELEMENT_DATA_V4 Elements;
-- } DHCP_SUBNET_ELEMENT_INFO_ARRAY_V4, *LPDHCP_SUBNET_ELEMENT_INFO_ARRAY_V4;
newtype SUBNET_ELEMENT_INFO_ARRAY_V4
    = SUBNET_ELEMENT_INFO_ARRAY_V4 (LengthBuffer SUBNET_ELEMENT_DATA_V4)

unwrap :: SUBNET_ELEMENT_INFO_ARRAY_V4 -> LengthBuffer SUBNET_ELEMENT_DATA_V4
unwrap (SUBNET_ELEMENT_INFO_ARRAY_V4 ia) = ia

infoArray :: DhcpStructure SUBNET_ELEMENT_INFO_ARRAY_V4
infoArray = newtypeDhcpStructure SUBNET_ELEMENT_INFO_ARRAY_V4 unwrap
    $ lengthBuffer (baseDhcpArray subnetElementData)
