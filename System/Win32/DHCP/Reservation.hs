{-# LANGUAGE RankNTypes #-}

module System.Win32.DHCP.Reservation
    ( Mapping (..)
    , Reservation (..)
    , reservation
    ) where

import Control.Applicative
import Foreign

import Data.Ip
import Data.Mac
import System.Win32.DHCP.CLIENT_UID
import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.Types (ClientType)

-- | A Reservation guarantees that a device with a given Mac address will
-- always be assigned to a particular IP address. A reservation is not the
-- same thing as a lease, and there are separate calls to work with both
-- objects.
--
-- This type corresponds to MSDN's DHCP_IP_RESERVATION_V4 structure.
--
-- > typedef struct _DHCP_IP_RESERVATION_V4 {
-- >   DHCP_IP_ADDRESS ReservedIpAddress;
-- >   DHCP_CLIENT_UID *ReservedForClient;
-- >   BYTE            bAllowedClientTypes;
-- > } DHCP_IP_RESERVATION_V4, *LPDHCP_IP_RESERVATION_V4;
data Reservation = Reservation
    { reservationMapping :: !Mapping
    , reservationType    :: !ClientType
    } deriving (Eq)

-- | A mapping between an IP and a MAC address. Each IP number may map to
-- only one MAC address, and each MAC address may map to only one IP number.
--
-- This is a separate type from `Reservation` for practical reasons. When
-- writing software to work with a DHCP server, `Reservation`'s
-- `ClientType` field is often not important. Without the `Mapping` type
-- defined here it would often be necessary to define a custom type in
-- each project.
data Mapping = Mapping
    { mappingMac :: !Mac
    , mappingIp  :: !Ip
    } deriving (Eq, Ord)

reservation :: DhcpStructure Reservation
reservation = DhcpStructure
    { peekDhcp = peekReservation
    , freeDhcpChildren = freeReservation
    , withDhcp' = withReservation'
    , sizeDhcp = 10
    }

peekReservation :: Ptr Reservation -> IO Reservation
peekReservation ptr = do
    pCuid <- peek $ ppCuid ptr
    mac <- macCuidDrop5 <$> peekDhcp clientUid pCuid
    addr <- peek pAddress
    clientType <- peekByteOff (castPtr ptr) 8
    return $ Reservation (Mapping mac addr) clientType
  where
    pAddress = castPtr ptr :: Ptr Ip

freeReservation :: (forall x. Ptr x -> IO ()) -> Ptr Reservation -> IO ()
freeReservation freefunc ptr = do
    peek (ppCuid ptr) >>= freeDhcp clientUid freefunc
    poke (ppCuid ptr) nullPtr

ppCuid :: Ptr Reservation -> Ptr (Ptr CLIENT_UID)
ppCuid ptr = castPtr ptr `plusPtr` 4

withReservation' :: Reservation -> Ptr Reservation
    -> IO r -> IO r
withReservation' (Reservation (Mapping mac address) clientType) ptr f =
    withMac mac $ \pCuid -> do
    poke (castPtr ptr) address
    pokeByteOff (castPtr ptr) 4 pCuid
    pokeByteOff (castPtr ptr) 8 clientType
    f
