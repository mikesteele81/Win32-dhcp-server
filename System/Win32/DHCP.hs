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
import Foreign
import Foreign.C.Types

import System.Win32.Types

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
      contextServer :: !String
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
    -> String
    -- ^ Unicode string that specifies the IP address or hostname of the DHCP
    -- server.
    -> SEARCH_INFO
    -- ^ Define how to lookup a client to delete. Only deleting based on
    -- IP addresses have been tested.
    -> IO ()
    -- ^ This action may throw exceptions. According to Microsoft's protocol
    -- specification an ERROR_DHCP_JET_ERROR (0x00004E2D) will be returned
    -- if no client was found on the server.
deleteClient api server si =
    withTString server $ \pserver ->
    withSearchInfo si $ \psi -> do
    failUnlessSuccess (unwords ["DeleteClientInfo", server, show si])
        $ c_DeleteClientInfo api pserver psi

-- | Perform a lookup operation for all client lease records within a
-- scope. This action corresponds to MSDN's DhcpEnumSubnetClientsV4 function.
enumClients :: DhcpApi
    -> Context
    -- ^ Specify which server and scope to search for client leases.
    -> IO [Client]
    -- ^ The empty list means that no client records exist for the provided
    -- subnet. An exception will be thrown if the provided server or subnet
    -- does not exist, or any other error occurs.
enumClients dhcp (Context server subnet) =
    enumSubnetClientsV4 dhcp server subnet

-- | Search the DHCP server for a lease matching the given search criteria.
-- `Nothing` is returned when no lease was found. This corresponds to MSDN's
-- DhcpGetClientInfoV4 function.
lookupClient :: DhcpApi
    -> String
    -- ^ According to MSDN this must specify the IP address of the server.
    -- Other functions (including this one) may or may not also accept
    -- a Unicode host name.
    -> SEARCH_INFO
    -- ^ Define how to lookup a client. Only searching based on an
    -- IP addresses has been tested.
    -> IO (Maybe Client)
    -- ^ A `Nothing` indicates that no client was found. An exception will
    -- be thrown if any internal error occurs. Though MSDN documents don't say
    -- so, an ERROR_DHCP_JET_ERROR (0x00004E2D) may be thrown if no client
    -- record is found. The reason I think this is that other functions are
    -- said to behave like this.
lookupClient api serverip si =
    withTString serverip $ \pserverip ->
    withSearchInfo si $ \psi ->
    with nullPtr $ \ppclientinfo -> do
    -- DhcpGetClientInfo takes a structure on the stack.
    siType <- peek (castPtr psi :: Ptr CInt)
    siPayload <- peekElemOff (castPtr psi :: Ptr (Ptr ())) 4
    failUnlessSuccess (unwords ["GetClientInfoV4", serverip, show si])
        $ c_GetClientInfoV4  api pserverip siType siPayload ppclientinfo
    pclientinfo <- peek ppclientinfo
    if pclientinfo == nullPtr
      then return Nothing
      else do
        clientinfo <- peekDhcp clientInfo pclientinfo
        freeDhcp clientInfo (rpcFreeMemory api) pclientinfo
        poke (castPtr pclientinfo) nullPtr
        return $ Just clientinfo

addReservation :: DhcpApi -> Context -> Mapping -> IO ()
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
    -- ^ Specify a DHCP reservation, BOOTP reservation, or both. This is untested.
    -> Bool
    -- ^ Specify whether any active leases for the reservation should be
    -- removed as well.
    -> IO ()
removeReservation dhcp (Context server subnet) mapping ct force =
    removeSubnetElementV4 dhcp server subnet
    (ReservedIps $ Reservation mapping ct) fForce
 where
   fForce = if force then FullForce else NoForce

enumSubnetClientsV4 :: DhcpApi -> String -> Ip -> IO [Client]
enumSubnetClientsV4 dhcp server subnet =
    withTString server $ \pServer ->
    -- inout parameter. Supply on successive calls to retreive more elements
    with 0 $ \pResumeHandle ->
    alloca $ \pElementsRead ->
    alloca $ \pElementsTotal ->
    with nullPtr $ \ppInfoArray -> do
    -- We have to call enumSubnetElementsV4 at least twice for a sucessfull run.
    -- Failure to use the returned resumeHandle may result in an internal access
    -- violation within RPCRT4.dll.
    let loop acc = do
            ret <- c_EnumSubnetClientsV4 dhcp pServer (toWord32 subnet)
                       pResumeHandle 0xFFFFFFFF ppInfoArray
                       pElementsRead pElementsTotal
            unless (elem ret [eRROR_SUCCESS, eRROR_MORE_DATA, eRROR_NO_MORE_ITEMS]) $ do
                failWith (unwords ["EnumSubnetClientsV4", server, showIp subnet]) ret
            pInfoArray <- peek ppInfoArray
            elems <- if pInfoArray == nullPtr
                     then return []
                     else do
                       SUBNET_CLIENT_INFO_ARRAY_V4 (LengthBuffer _ elems) <- peekDhcp clientInfoArray pInfoArray
                       freeDhcp clientInfoArray (rpcFreeMemory dhcp) pInfoArray
                       poke ppInfoArray nullPtr
                       return elems
            elementsTotal <- peek pElementsTotal
            elementsRead <- peek pElementsRead
            if (ret == eRROR_NO_MORE_ITEMS || (ret == eRROR_SUCCESS && elementsTotal == elementsRead))
              then return (eRROR_SUCCESS, elems:acc)
              else loop (elems:acc)

    (ret, revelemss) <- loop []
    failUnlessSuccess (unwords ["EnumSubnetClientsV4", server, showIp subnet])
        $ return ret
    return $ concat $ reverse revelemss
  where
    eRROR_SUCCESS       = 0x00000000
    eRROR_NO_MORE_ITEMS = 0x00000103
    eRROR_MORE_DATA     = 0x000000ea

enumSubnetElementsV4 :: DhcpApi -> String -> Ip -> CInt
    -> IO [SUBNET_ELEMENT_DATA_V4]
enumSubnetElementsV4 dhcp server subnet elementType =
    withTString server $ \pServer ->
    -- inout parameter. Supply on successive calls to retreive more elements
    with 0 $ \pResumeHandle ->
    alloca $ \pElementsRead ->
    alloca $ \pElementsTotal ->
    with nullPtr $ \ppEnumElementInfoArray -> do
    -- We have to call enumSubnetElementsV4 at least twice for a sucessfull run.
    -- Failure to use the returned resumeHandle may result in an internal access
    -- violation within RPCRT4.dll.
    let loop acc = do
            ret <- c_EnumSubnetElementsV4 dhcp pServer (toWord32 subnet) elementType
                       pResumeHandle 0xFFFFFFFF ppEnumElementInfoArray
                       pElementsRead pElementsTotal
            unless (elem ret [eRROR_SUCCESS, eRROR_MORE_DATA, eRROR_NO_MORE_ITEMS]) $ do
                failWith (unwords ["EnumSubnetElementsV4", server
                                  , showIp subnet, show elementType]) ret
            pEnumElementInfoArray <- peek ppEnumElementInfoArray
            elems <- if pEnumElementInfoArray == nullPtr
                     then return []
                     else do
                       SUBNET_ELEMENT_INFO_ARRAY_V4 (LengthBuffer _ elems) <- peekDhcp infoArray pEnumElementInfoArray
                       freeDhcp infoArray (rpcFreeMemory dhcp) pEnumElementInfoArray
                       poke ppEnumElementInfoArray nullPtr
                       return elems
            elementsTotal <- peek pElementsTotal
            if (ret == eRROR_NO_MORE_ITEMS || (ret == eRROR_SUCCESS && elementsTotal == 0))
              then return (eRROR_SUCCESS, elems:acc)
              else loop (elems:acc)

    (ret, revelemss) <- loop []
    failUnlessSuccess (unwords ["EnumSubnetElementsV4", server
                               , showIp subnet, show elementType])
        $ return ret
    return $ concat $ reverse revelemss
  where
    eRROR_SUCCESS       = 0x00000000
    eRROR_NO_MORE_ITEMS = 0x00000103
    eRROR_MORE_DATA     = 0x000000ea


addSubnetElementV4 :: DhcpApi -> String -> Ip -> SUBNET_ELEMENT_DATA_V4 -> IO ()
addSubnetElementV4 dhcp server subnet elementData =
    withTString server $ \pServer ->
    withDhcp subnetElementData elementData $ \pElementData ->
    failUnlessSuccess (unwords ["AddSubnetElementsV4", server, showIp subnet])
    $ c_AddSubnetElementV4 dhcp pServer (toWord32 subnet) pElementData

-- | Remove an IPv4 subnet element from an IPv4 subnet defined on the DHCPv4
--   server.
removeSubnetElementV4
    :: DhcpApi
    -> String -- ^ Unicode string that specifies the IP address, such as
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
    -> IO () -- ^ Following the convention of the Win32 package, an exeption
             --   indicating the win32 error code will be raised if the
             --   underlying API call returns anything other than
             --   ERROR_SUCCESS.
removeSubnetElementV4 dhcp server subnet elementData forceFlag =
    withTString server $ \pServer ->
    withDhcp subnetElementData elementData $ \pElementData ->
    failUnlessSuccess (unwords ["RemoveSubnetElementV4", server, showIp subnet
                               , (show . elementTypeOf) elementData])
    $ c_RemoveSubnetElementV4 dhcp pServer (toWord32 subnet) pElementData
          (fromIntegral . fromEnum $ forceFlag)
