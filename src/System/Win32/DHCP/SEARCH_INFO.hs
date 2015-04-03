{-# LANGUAGE OverloadedStrings #-}

module System.Win32.DHCP.SEARCH_INFO
  ( SEARCH_INFO_TYPE
  , SEARCH_INFO (..)
  , withSearchInfo
  ) where

import qualified Data.Text as T

import Data.Ip
import Data.Mac
import Import
import System.Win32.DHCP.CLIENT_UID

-- typedef enum _DHCP_CLIENT_SEARCH_TYPE { 
--   DhcpClientIpAddress,
--   DhcpClientHardwareAddress,
--   DhcpClientName
-- } DHCP_SEARCH_INFO_TYPE, *LPDHCP_SEARCH_INFO_TYPE;
type SEARCH_INFO_TYPE = CInt

-- | Filter criteria used in actions that look up reservation or lease
-- records.
--
-- > typedef struct _DHCP_CLIENT_SEARCH_INFO {
-- >   DHCP_SEARCH_INFO_TYPE SearchType;
-- >   union {
-- >     DHCP_IP_ADDRESS ClientIpAddress;
-- >     DHCP_CLIENT_UID ClientHardwareAddress;
-- >     LPWSTR          ClientName;
-- >   } SearchInfo;
-- > } DHCP_SEARCH_INFO, *LPDHCP_SEARCH_INFO;
data SEARCH_INFO
  -- | Search based on an IP address. All scopes are searched. It should
  -- not be possible for multiple records to exist.
  = ClientIpAddress       !Ip
  -- | Search based on a subnet and MAC address. This method of searching
  -- has not been tested.
  | ClientHardwareAddress !Mac
  -- | Search based on a client's name. Multiple records may exist, and
  -- what happens in that case will depend on the function being called.
  -- This method of searching has not been tested.
  | ClientName            !String

instance Show SEARCH_INFO where
  show (ClientIpAddress ip) = T.unpack $ "ClientIpAddress " <> showIp ip
  show (ClientHardwareAddress mac) = T.unpack $ "ClientHardwareAddress " <> showMac ":" mac
  show (ClientName name) = "ClientName " ++ name

siTypeOf :: SEARCH_INFO -> SEARCH_INFO_TYPE
siTypeOf (ClientIpAddress _) = 0
siTypeOf (ClientHardwareAddress _) = 1
siTypeOf (ClientName _) = 2

-- Allocate 12 because a SEARCH_INFO.SearchInfo member's in-structure alignment is 4 with a size of 8.
withSearchInfo :: SEARCH_INFO -> (Ptr SEARCH_INFO -> IO r) -> IO r
withSearchInfo si f = allocaBytes 12 $ \ptr -> do
    let pX = ptr `plusPtr` 4
    poke (castPtr ptr) $ siTypeOf si
    case si of
      ClientIpAddress x -> poke (castPtr pX) x >> f ptr
      ClientHardwareAddress m -> withMac m $ \pm -> copyBytes (castPtr pX) pm 8 >> f ptr
      -- We're preserving API compatibility here. A future version of
      -- Win32-dhcp-server will used Text values.
      ClientName str -> withTString (T.pack str)
          $ \pstr -> copyBytes (castPtr pX) pstr 4 >> f ptr
