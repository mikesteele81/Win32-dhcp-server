module System.Win32.DHCP.CLIENT_UID
    ( CLIENT_UID (..)
    , clientUid
    , macCuid
    , macCuidDrop5
    , withMac
    ) where

import Foreign
import System.Win32.Types

import Data.Mac
import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.LengthBuffer

-- typedef struct _DHCP_BINARY_DATA {
--   DWORD DataLength;
--   BYTE  *Data;
-- } DHCP_BINARY_DATA, *LPDHCP_BINARY_DATA, DHCP_CLIENT_UID;

-- Byte 0 - 3: The result of a binary AND on the IP address and the subnet
--             mask in reverse order.
-- Byte 4: Hardware identifier. This value is always 0x01.
-- Byte 5 - 10: The Mac address of the client.
newtype CLIENT_UID = CLIENT_UID (LengthBuffer BYTE)

unwrap :: CLIENT_UID -> LengthBuffer BYTE
unwrap (CLIENT_UID uid) = uid

clientUid :: DhcpStructure CLIENT_UID
clientUid = newtypeDhcpStructure CLIENT_UID unwrap
    $ lengthBuffer (basicDhcpArray storableDhcpStructure)

-- Functions returning a CLIENT_UID often have the first 5 bytes hold
-- information about the subnet being used.
macCuidDrop5 :: CLIENT_UID -> Mac
macCuidDrop5 (CLIENT_UID (LengthBuffer _ bytes)) = fromOctets a b c d e f
  where
    [a, b, c, d, e, f] = drop 5 bytes

macCuid :: CLIENT_UID -> Mac
macCuid (CLIENT_UID (LengthBuffer _ bytes)) = fromOctets a b c d e f
  where
    [a, b, c, d, e, f] = bytes

fromMac :: Mac -> CLIENT_UID
fromMac mac = CLIENT_UID (LengthBuffer 6 [a, b, c, d, e, f])
  where
    (a, b, c, d, e, f) = toOctets mac

-- |When creating CLIENT_UID structures in memory we only need 6 bytes
-- representing the Mac address. This is contrary to MSDN documentation, which
-- states that there should be 11 bytes, with the first 5 being constructed
-- from the IP and subnet.
withMac :: Mac -> (Ptr CLIENT_UID -> IO b) -> IO b
withMac mac f = withDhcp clientUid (fromMac mac) f
