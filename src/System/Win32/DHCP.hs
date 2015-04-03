{-# LANGUAGE OverloadedStrings #-}

-- |
-- module: System.Win32.DHCP
-- copyright: (c) Michael Steele, 2014
-- license: BSD3
-- maintainer: mikesteele81@gmail.com
-- stability: experimental
-- portability: Windows
module System.Win32.DHCP
    ( DhcpApi ()
    , loadDHCP
    -- * General Types
    , Context (..)
    , ClientType (..)
    , DATE_TIME (..)
    , HOST_INFO (..)
    , SEARCH_INFO (..)
    -- * Leases
    , Client (..)
    , enumClients
    , lookupClient
    , deleteClient
    -- * Reservations
    , Mapping (..)
    , Reservation(..)
    , addReservation
    , enumReservations
    , removeReservation
    ) where

import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

import Import
import Data.Ip
import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.HOST_INFO
import System.Win32.DHCP.Internal
import System.Win32.DHCP.Client
import System.Win32.DHCP.LengthBuffer
import System.Win32.DHCP.Reservation
import System.Win32.DHCP.SEARCH_INFO
import System.Win32.DHCP.SUBNET_CLIENT_INFO_ARRAY_V4
import System.Win32.DHCP.SUBNET_ELEMENT_DATA_V4
import System.Win32.DHCP.SUBNET_ELEMENT_INFO_ARRAY_V4
import System.Win32.DHCP.Types

-- | A Context defines which server and scope within that server a command
-- refers to.  Microsoft's DHCP server supports multiple scopes. This allows
-- different configurations to be sent to devices based on their hardware
-- (MAC) address.  Scopes are identified by their network address.
data Context = Context
    { -- | The DHCP server management API uses an RPC mechanism to control the
      -- server.
      contextServer :: !Text
    , -- | Scopes are identified by a subnet identifier. This is useful in
      -- cases where multiple scopes are defined, but still required in the
      -- common case of a single scope.
      contextSubnet :: !Ip
    } deriving (Eq, Ord)

-- | Delete an active DHCP lease from the server.
-- The `SEARCH_INFO` argument determines which search criteria
-- to use. Searching by name will delete all active leases
-- with that name. This action corresponds to MSDN's DhcpDeleteClientInfoV4
-- function.
deleteClient :: DhcpApi
    -> Text
    -- ^ String which specifies the IP address or hostname of the DHCP
    -- server.
    -> SEARCH_INFO
    -- ^ Define how to lookup a client to delete. Only deleting based on
    -- IP addresses have been tested.
    -> IO ()
    -- ^ This function will throw an 'Win32Exception' when the internal Win32
    -- call returnes an error condition. MSDN lists the following exceptions,
    -- but others might be thrown as well:
    --
    --   ['DhcpReservedClient'] The specified DHCP client is a reserved DHCP
    --   client.
    --
    --   ['DhcpJetError'] An error occurred while accessing the DHCP server
    --   database, or the client entry is not present in the database.
deleteClient api server si =
    withTString server $ \pserver ->
    withSearchInfo si $ \psi ->
    failUnlessSuccess "DeleteClientInfo"
    $ c_DeleteClientInfo api pserver psi

-- | Perform a lookup operation for all client lease records within a
-- scope. This action corresponds to MSDN's DhcpEnumSubnetClientsV4 function.
enumClients :: DhcpApi
    -> Context
    -- ^ Specify which server and scope to search for client leases.
    -> IO [Client]
    -- ^ The empty list means that no client records exist for the provided
    -- subnet. This function will throw an 'Win32Exception' when the
    -- internal Win32 call returnes an error condition. MSDN lists the
    -- following exceptions, but others might be thrown as well:
    --
    --   ['DhcpJetError'] An error occurred while accessing the DHCP server
    --   database.
enumClients dhcp (Context server subnet) =
    enumSubnetClientsV4 dhcp server subnet

-- | Search the DHCP server for a lease matching the given search criteria.
-- `Nothing` is returned when no lease was found. This corresponds to MSDN's
-- DhcpGetClientInfoV4 function.
lookupClient :: DhcpApi
    -> Text
    -- ^ According to MSDN this must specify the IP address of the server.
    -- Other functions (including this one) may or may not also accept
    -- a Unicode host name.
    -> SEARCH_INFO
    -- ^ Define how to lookup a client. Only searching based on an
    -- IP addresses has been tested.
    -> IO (Maybe Client)
    -- ^ A 'Nothing' indicates that no client was found. This function will
    -- throw an 'Win32Exception' when the internal Win32 call returnes an
    -- error condition. MSDN lists the following exceptions, but others might
    -- be thrown as well:
    --
    --   ['DhcpJetError'] An error occurred while accessing the DHCP server
    --   database.
lookupClient api serverip si =
    withTString serverip $ \pserverip ->
    withSearchInfo si $ \psi ->
    with nullPtr $ \ppclientinfo -> do
    -- DhcpGetClientInfo takes a structure on the stack.
    siType <- peek (castPtr psi :: Ptr CInt)
    siPayload <- peekElemOff (castPtr psi :: Ptr (Ptr ())) 4
    failUnlessSuccess "GetClientInfoV4"
        $ c_GetClientInfoV4  api pserverip siType siPayload ppclientinfo

    -- Extract client information from out parameter and free the memory
    -- 'Nothing' will be returned the case where GetClientInfoV4 returns a
    -- null pointer.
    scrubWith ppclientinfo $ \pclientinfo -> do
        clientinfo <- peekDhcp clientInfo pclientinfo
        freeDhcp clientInfo (rpcFreeMemory api) pclientinfo
        return clientinfo

addReservation :: DhcpApi -> Context -> Mapping
    -> IO ()
    -- ^ This function will throw an 'Win32Exception' when the internal Win32
    -- call returnes an error condition. MSDN lists the following exceptions,
    -- but others might be thrown as well:
    --
    --   ['DhcpSubnetNotPresent'] The specified IPv4 subnet does not exist
    --   on the DHCP server.
    --
    --   ['DhcpJetError'] An error occurred while accessing the DHCP server
    --   database.
    --
    --   ['DhcpNotReservedClient'] The specified DHCP client is not a
    --   reserved client.
    --
    --   ['DhcpInvalidRange'] The specified IPv4 range does not match an
    --   existing IPv4 range.
    --
    --   ['ScopeRangePolicyChangeConflict'] An IP address range is
    --   configured for a policy in this scope. This operation cannot be
    --   performed on the scope IP address range until the policy IP address
    --   range is suitably modified.
addReservation dhcp (Context server subnet) mapping =
    addSubnetElementV4 dhcp server subnet
    $ ReservedIps (Reservation mapping Both)

enumReservations :: DhcpApi -> Context -> IO [Reservation]
enumReservations dhcp (Context server subnet) = do
    ex <- enumSubnetElementsV4 dhcp server subnet 2
    res <- flip mapM ex $ \element -> do
        case element of
          ReservedIps res -> return res
          _ -> error "bug in Win32 API."
    return res

-- | Remove a reservation from the server
removeReservation :: DhcpApi -> Context -> Mapping
    -> ClientType
    -- ^ Specify a DHCP reservation, BOOTP reservation, or both. This is
    -- untested.
    -> Bool
    -- ^ Specify whether any active leases for the reservation should be
    -- removed as well.
    -> IO ()
    -- ^ This function will throw an 'Win32Exception' when the internal Win32
    -- call returnes an error condition. MSDN lists the following exceptions,
    -- but others might be thrown as well:
    --
    --   ['DhcpSubnetNotPresent'] The specified IPv4 subnet does not exist
    --   on the DHCP server.
    --
    --   ['DhcpElementCantRemove'] Failure can occur for any number of
    --   reasons.
    --
    --   ['DhcpJetError'] An error occurred while accessing the DHCP server
    --   database.
    --
    --   ['DhcpInvalidRange'] The specified IPv4 range does not match an
    --   existing IPv4 range.
    --
    --   ['ScopeRangePolicyChangeConflict'] An IP address range is
    --   configured for a policy in this scope. This operation cannot be
    --   performed on the scope IP address range until the policy IP address
    --   range is suitably modified.
removeReservation dhcp (Context server subnet) mapping ct force =
    removeSubnetElementV4 dhcp server subnet
    (ReservedIps $ Reservation mapping ct) fForce
 where
   fForce = if force then FullForce else NoForce

enumSubnetClientsV4 :: DhcpApi -> Text -> Ip -> IO [Client]
enumSubnetClientsV4 dhcp server subnet =
    withTString server $ \pServer ->
    -- inout parameter. Supply on successive calls to retreive more elements
    with 0 $ \pResumeHandle ->
    alloca $ \pElementsRead ->
    alloca $ \pElementsTotal ->
    with nullPtr $ \ppInfoArray -> do
    -- We have to call enumSubnetElementsV4 at least twice for a sucessfull
    -- run.  Failure to use the returned resumeHandle may result in an
    -- internal access violation within RPCRT4.dll.
    let loop acc = do
            ret <- fromDWORD <$> c_EnumSubnetClientsV4 dhcp pServer (toWord32 subnet)
                                   pResumeHandle 0xFFFFFFFF ppInfoArray
                                   pElementsRead pElementsTotal
            unless (elem ret [Success, MoreData, NoMoreItems])
                $ failWith "EnumSubnetClientsV4" ret

            melems <- scrubWith ppInfoArray $ \pInfoArray -> do
                SUBNET_CLIENT_INFO_ARRAY_V4 (LengthBuffer _ elems) <- peekDhcp clientInfoArray pInfoArray
                freeDhcp clientInfoArray (rpcFreeMemory dhcp) pInfoArray
                return elems
            let elems = fromMaybe [] melems

            if (ret == NoMoreItems || ret == Success)
              then return (elems:acc)
              else loop (elems:acc)

    revelemss <- loop []
    return $ concat $ reverse revelemss

enumSubnetElementsV4 :: DhcpApi -> Text -> Ip -> CInt
    -> IO [SUBNET_ELEMENT_DATA_V4]
enumSubnetElementsV4 dhcp server subnet elementType =
    withTString server $ \pServer ->
    -- inout parameter. Supply on successive calls to retreive more elements
    with 0 $ \pResumeHandle ->
    alloca $ \pElementsRead ->
    alloca $ \pElementsTotal ->
    with nullPtr $ \ppInfoArray -> do
    -- We have to call enumSubnetElementsV4 at least twice for a sucessfull
    -- run.  Failure to use the returned resumeHandle may result in an
    -- internal access violation within RPCRT4.dll.
    let loop acc = do
            ret <- fromDWORD <$> c_EnumSubnetElementsV4 dhcp pServer (toWord32 subnet) elementType
                                   pResumeHandle 0xFFFFFFFF ppInfoArray
                                   pElementsRead pElementsTotal
            unless (elem ret [Success, MoreData, NoMoreItems])
                $ failWith "EnumSubnetElementsV4" ret

            melems <- scrubWith ppInfoArray $ \pInfoArray -> do
                SUBNET_ELEMENT_INFO_ARRAY_V4 (LengthBuffer _ elems) <- peekDhcp infoArray pInfoArray
                freeDhcp infoArray (rpcFreeMemory dhcp) pInfoArray
                return elems
            let elems = fromMaybe [] melems

            -- Intentionally ignore elementsTotal. Microsoft's documentation
            -- on how this should work doesn't seem right. In my testing
            -- elementsTotal always returns 0x7fffffff until the last loop, at
            -- which time it always matches elementsRead.
            if (ret == NoMoreItems || ret == Success)
              then return (elems:acc)
              else loop (elems:acc)

    revelemss <- loop []
    return $ concat $ reverse revelemss

addSubnetElementV4 :: DhcpApi -> Text -> Ip -> SUBNET_ELEMENT_DATA_V4 -> IO ()
addSubnetElementV4 dhcp server subnet elementData =
    withTString server $ \pServer ->
    withDhcp subnetElementData elementData $ \pElementData ->
    failUnlessSuccess "AddSubnetElementsV4"
    $ c_AddSubnetElementV4 dhcp pServer (toWord32 subnet) pElementData

-- | Remove an IPv4 subnet element from an IPv4 subnet defined on the DHCPv4
--   server.
removeSubnetElementV4
    :: DhcpApi
    -> Text   -- ^ Unicode string that specifies the IP address, such as
              --   \"123.456.789.012\", or hostname, such as \"server\",
              --   of the DHCP server.
    -> Ip -- ^ 'DWORD' value that specifies the IP address of the
                  --   subnet gateway and uniquely identifies it. As an
                  --   example, with the above IP address and a subnet mask
                  --   of 255.255.255.0, the subnet would be 123.456.789.0.
                  --   This must then be converted to a 'DWORD' value of
                  --   2,093,683,968.
    -> SUBNET_ELEMENT_DATA_V4 -- ^ 'DHCP_SUBNET_ELEMENT_DATA_V4' structure
                              --   that contains information used to find the
                              --   element that will be removed from subnet
                              --   specified in SubnetAddress.
    -> FORCE_FLAG -- ^ DHCP_FORCE_FLAG enumeration value that indicates
                  --   whether or not the clients affected by the removal of
                  --   the subnet element should also be deleted.
                  --
                  --   Note If the flag is set to DhcpNoForce and this subnet
                  --   has served an IPv4 address to DHCPv4/BOOTP clients, the
                  --   IPv4 range is not deleted; conversely, if the flag is
                  --   set to DhcpFullForce, the IPv4 range is deleted along
                  --   with the DHCPv4 client lease record on the DHCPv4
                  --   server.
    -> IO ()
    -- ^ This function will throw an 'Win32Exception' when the internal Win32
    -- call returnes an error condition. MSDN lists the following exceptions,
    -- but others might be thrown as well:
    --
    --   ['DhcpSubnetNotPresent'] The specified IPv4 subnet does not exist
    --   on the DHCP server.
    --
    --   ['DhcpElementCantRemove'] Failure can occur for any number of
    --   reasons.
    --
    --   ['DhcpJetError'] An error occurred while accessing the DHCP server
    --   database.
    --
    --   ['DhcpInvalidRange'] The specified IPv4 range does not match an
    --   existing IPv4 range.
    --
    --   ['ScopeRangePolicyChangeConflict'] An IP address range is
    --   configured for a policy in this scope. This operation cannot be
    --   performed on the scope IP address range until the policy IP address
    --   range is suitably modified.
removeSubnetElementV4 dhcp server subnet elementData forceFlag =
    withTString server $ \pServer ->
    withDhcp subnetElementData elementData $ \pElementData ->
    failUnlessSuccess "RemoveSubnetElementV4"
    $ c_RemoveSubnetElementV4 dhcp pServer (toWord32 subnet) pElementData
          (fromIntegral . fromEnum $ forceFlag)
