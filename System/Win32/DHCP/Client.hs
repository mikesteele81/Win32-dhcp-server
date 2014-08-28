{-# LANGUAGE RankNTypes #-}

module System.Win32.DHCP.Client
  ( Client (..)
  , clientInfo
  ) where

import Control.Applicative
import Foreign

import System.Win32.Types

import Data.Ip
import Data.Mac
import System.Win32.DHCP.CLIENT_UID
import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.HOST_INFO
import System.Win32.DHCP.Types
import Utils

-- | Information about an active lease. This type corresponds to
-- MSDN's DHCP_CLIENT_INFO_V4 structure.
--
-- > typedef struct _DHCP_CLIENT_INFO_V4 {
-- >   DHCP_IP_ADDRESS ClientIpAddress;
-- >   DHCP_IP_MASK    SubnetMask;
-- >   DHCP_CLIENT_UID ClientHardwareAddress;
-- >   LPWSTR          ClientName;
-- >   LPWSTR          ClientComment;
-- >   DATE_TIME       ClientLeaseExpires;
-- >   DHCP_HOST_INFO  OwnerHost;
-- >   BYTE            bClientType;
-- > } DHCP_CLIENT_INFO_V4, *LPDHCP_CLIENT_INFO_V4;
data Client = Client
    { clientIp              :: !Ip
    , clientSubnetMask      :: !Ip
    , clientHardwareAddress :: !Mac
    , clientName            :: Maybe String
    , clientComment         :: Maybe String
    , clientLeaseExpires    :: !DATE_TIME
    -- ^ MSDN: The date and time the DHCP client lease will expire, in UTC
    -- time.
    --
    -- I don't know of any available functions to work with a `DATE_TIME`.
    , clientOwnerHost       :: !HOST_INFO
    -- ^ Information on the DHCP server that assigned the lease to the client.
    , clientType            :: !ClientType
    }
 
clientInfo :: DhcpStructure Client
clientInfo = DhcpStructure
    { peekDhcp = peekClientInfoV4
    , freeDhcpChildren = freeClientInfoV4
    , withDhcp' = withClientInfo'
    -- 12-byte alignment because of the inlined HOST_INFO struct
    , sizeDhcp = 48
    }

withClientInfo' :: Client -> Ptr Client -> IO r -> IO r
withClientInfo' c ptr f = 
    -- The Mac is inlined, so we'll need to copy pmacsrc into pmac
    withMac     (clientHardwareAddress c) $ \pcuidsrc ->
    withMaybeTString (clientName c)       $ \pclientName ->
    withMaybeTString (clientComment c)    $ \pclientComment ->
    withDhcp' hostInfo   (clientOwnerHost c) (pownerHost ptr) $ do
    poke (pclientIP ptr)     $ clientIp c
    poke (psubnetMask ptr)   $ clientSubnetMask c
    -- we can't use the Storable instance for Mac here.
    copyBytes (pmac ptr) pcuidsrc $ sizeDhcp clientUid
    poke (ppclientName ptr)    pclientName
    poke (ppclientComment ptr) pclientComment
    poke (pleaseExpires ptr) $ clientLeaseExpires c
     -- Owner host has already been poked
    poke (pclientType ptr)   $ clientType c
    f

peekClientInfoV4 :: Ptr Client -> IO Client
peekClientInfoV4 ptr = Client
    <$> (peek $ pclientIP ptr)
    <*> (peek $ psubnetMask ptr)
    <*> (macCuid <$> peekDhcp clientUid (pmac ptr))
    <*> (peek (ppclientName ptr) >>= peekMaybeTString)
    <*> (peek (ppclientComment ptr) >>= peekMaybeTString)
    <*> (peek $ pleaseExpires ptr)
    <*> (peekDhcp hostInfo $ pownerHost ptr)
    <*> (peek $ pclientType ptr)

freeClientInfoV4 :: (forall a. Ptr a -> IO ()) -> Ptr Client -> IO ()
freeClientInfoV4 freefunc ptr = do
    -- The client_uid is inlined into the client_info structure.
    freeDhcpChildren clientUid freefunc $ pmac ptr
    freefunc `scrubbing_` ppclientName ptr
    freefunc `scrubbing_` ppclientComment ptr
    -- The HOST_INFO structure is inlined within the ClientInfoV4, so we don't
    -- have to free its main pointer.
    freeDhcpChildren hostInfo freefunc $ pownerHost ptr

pclientIP :: Ptr Client -> Ptr Ip
pclientIP = castPtr

psubnetMask :: Ptr Client -> Ptr Ip
psubnetMask ptr = castPtr ptr `plusPtr` 4

-- The client_uid is inlined into the client_info structure.
pmac :: Ptr Client -> Ptr CLIENT_UID
pmac ptr = castPtr ptr `plusPtr` 8

ppclientName :: Ptr Client -> Ptr LPWSTR
ppclientName ptr = castPtr ptr `plusPtr` 16

ppclientComment :: Ptr Client -> Ptr LPWSTR
ppclientComment ptr = castPtr ptr `plusPtr` 20

pleaseExpires :: Ptr Client -> Ptr DATE_TIME
pleaseExpires ptr = castPtr ptr `plusPtr` 24

-- the HOST_INFO is inlined into the CLIENT_INFO structure
pownerHost :: Ptr Client -> Ptr HOST_INFO
pownerHost ptr = castPtr ptr `plusPtr` 32

pclientType :: Ptr Client -> Ptr ClientType
pclientType ptr = castPtr ptr `plusPtr` 44
