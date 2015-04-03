{-# LANGUAGE RankNTypes #-}

module System.Win32.DHCP.HOST_INFO
    ( HOST_INFO (..)
    , hostInfo
    ) where

import Data.Ip
import Import
import System.Win32.DHCP.DhcpStructure

-- typedef struct _DHCP_HOST_INFO {
--   DHCP_IP_ADDRESS IpAddress;
--   LPWSTR          NetBiosName;
--   LPWSTR          HostName;
-- } DHCP_HOST_INFO, *LPDHCP_HOST_INFO;
data HOST_INFO = HOST_INFO !Ip (Maybe String) (Maybe String)


hostInfo :: DhcpStructure HOST_INFO
hostInfo = DhcpStructure
    { peekDhcp = peekHostInfo
    , freeDhcpChildren = freeHostInfoChildren
    , withDhcp' = withHostInfo'
    , sizeDhcp = 12
    }

peekHostInfo :: Ptr HOST_INFO -> IO HOST_INFO
peekHostInfo ptr = do
    pnetBiosName <- peek $ ppnetBiosName ptr
    phostName <- peek $ pphostName ptr
    HOST_INFO <$> peek (castPtr ptr) <*> peekMaybeTString pnetBiosName
              <*> peekMaybeTString phostName

withHostInfo' :: HOST_INFO -> Ptr HOST_INFO -> IO r -> IO r
withHostInfo' (HOST_INFO ip netbiosname hostname) ptr f =
    withMaybeTString netbiosname $ \pnetbiosname ->
    withMaybeTString hostname    $ \phostname -> do
    poke (castPtr ptr) ip
    pokeByteOff (castPtr ptr) 4 pnetbiosname
    pokeByteOff (castPtr ptr) 8 phostname
    f

freeHostInfoChildren :: (forall x. Ptr x -> IO ()) -> Ptr HOST_INFO -> IO ()
freeHostInfoChildren rpcfree phi = do
    rpcfree `scrubbing_` ppnetBiosName phi
    rpcfree `scrubbing_` pphostName phi

ppnetBiosName :: Ptr HOST_INFO -> Ptr LPWSTR
ppnetBiosName ptr = castPtr ptr `plusPtr` 4

pphostName :: Ptr HOST_INFO -> Ptr LPWSTR
pphostName ptr = castPtr ptr `plusPtr` 8
