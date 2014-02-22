{-# LANGUAGE ForeignFunctionInterface #-}

module System.Win32.DHCP.Internal where

import Foreign
import Foreign.C
import System.Win32.DLL
import System.Win32.Types

import System.Win32.DHCP.Client
import System.Win32.DHCP.SEARCH_INFO
import System.Win32.DHCP.SUBNET_CLIENT_INFO_ARRAY_V4
import System.Win32.DHCP.SUBNET_ELEMENT_INFO_ARRAY_V4
import System.Win32.DHCP.SUBNET_ELEMENT_DATA_V4
import System.Win32.DHCP.Types

-- DWORD DHCP_API_FUNCTION DhcpDeleteClientInfo(
--   _In_  DHCP_CONST WCHAR *ServerIpAddress,
--   _In_  DHCP_CONST DHCP_SEARCH_INFO *ClientInfo
-- );
type DeleteClientInfo = CWString -> Ptr SEARCH_INFO -> IO DWORD
foreign import stdcall "dynamic"
    mkDeleteClientInfo :: FunPtr DeleteClientInfo -> DeleteClientInfo

-- DWORD DHCP_API_FUNCTION DhcpEnumSubnetClientsV4(
--   _In_     DHCP_CONST WCHAR *ServerIpAddress,
--   _In_     DHCP_IP_ADDRESS SubnetAddress,
--   _Inout_  DHCP_RESUME_HANDLE *ResumeHandle,
--   _In_     DWORD PreferredMaximum,
--   _Out_    LPDHCP_CLIENT_INFO_ARRAY_V4 *ClientInfo,
--   _Out_    DWORD *ClientsRead,
--   _Out_    DWORD *ClientsTotal
-- );
type EnumSubnetClientsV4 = CWString -> Word32 -> Ptr RESUME_HANDLE
    -> DWORD -> Ptr (Ptr SUBNET_CLIENT_INFO_ARRAY_V4) -> Ptr DWORD
    -> Ptr DWORD -> IO DWORD
foreign import stdcall "dynamic"
    mkEnumSubnetClientsV4 :: FunPtr EnumSubnetClientsV4
        -> EnumSubnetClientsV4

-- DWORD DHCP_API_FUNCTION DhcpEnumSubnetElementsV4(
--   __in     DHCP_CONST WCHAR *ServerIpAddress,
--   __in     DHCP_IP_ADDRESS SubnetAddress,
--   __in     DHCP_SUBNET_ELEMENT_TYPE EnumElementType,
--   __inout  DHCP_RESUME_HANDLE *ResumeHandle,
--   __in     DWORD PreferredMaximum,
--   __out    LPDHCP_SUBNET_ELEMENT_INFO_ARRAY_V4 *EnumElementInfo,
--  __out    DWORD *ElementsRead,
--  __out    DWORD *ElementsTotal
-- );
type EnumSubnetElementsV4 = CWString -> Word32 -> CInt -> Ptr RESUME_HANDLE
    -> DWORD -> Ptr (Ptr SUBNET_ELEMENT_INFO_ARRAY_V4) -> Ptr DWORD
    -> Ptr DWORD -> IO DWORD
foreign import stdcall "dynamic"
    mkEnumSubnetElementsV4 :: FunPtr EnumSubnetElementsV4
        -> EnumSubnetElementsV4

-- DWORD DHCP_API_FUNCTION DhcpGetClientInfoV4(
--   _In_   DHCP_CONST WCHAR ServerIpAddress,
--   _In_   DHCP_CONST DHCP_SEARCH_INFO SearchInfo,
--   _Out_  LPDHCP_CLIENT_INFO_V4 *ClientInfo
-- );
type GetClientInfoV4 = CWString -> SEARCH_INFO_TYPE -> Ptr ()
    -> Ptr (Ptr Client) -> IO DWORD
foreign import stdcall "dynamic"
    mkGetClientInfoV4 :: FunPtr GetClientInfoV4 -> GetClientInfoV4

-- | Foreign wrapper to DhcpRemoveSubnetElementV4.
-- 
--   The DhcpRemoveSubnetElementV4 function removes an IPv4 subnet element
--   from an IPv4 subnet defined on the DHCPv4 server. The function extends
--   the functionality provided by DhcpRemoveSubnetElement by allowing the
--   specification of a subnet that contains client type (DHCP or BOOTP)
--   information.
-- 
--   See <http://msdn.microsoft.com/en-us/library/ee309533(v=VS.85).aspx> for
--   official documentation.
-- 
-- @
--   DWORD DHCP_API_FUNCTION DhcpRemoveSubnetElementV4(
--     __in  DHCP_CONST WCHAR *ServerIpAddress,
--     __in  DHCP_IP_ADDRESS SubnetAddress,
--     __in  DHCP_CONST DHCP_SUBNET_ELEMENT_DATA_V4 *RemoveElementInfo,
--     __in  DHCP_FORCE_FLAG ForceFlag
--   );
-- @
type RemoveSubnetElementV4 = CWString -> Word32 -> Ptr SUBNET_ELEMENT_DATA_V4
    -> CInt -> IO DWORD
foreign import stdcall "dynamic"
    mkRemoveSubnetElementV4 :: FunPtr RemoveSubnetElementV4
        -> RemoveSubnetElementV4

-- DWORD DhcpAddSubnetElementV4(
--   __in  DHCP_CONST WCHAR *ServerIpAddress,
--   __in  DHCP_IP_ADDRESS SubnetAddress,
--   __in  DHCP_CONST DHCP_SUBNET_ELEMENT_DATA_V4 AddElementInfo
-- );
type AddSubnetElementV4 = CWString -> Word32 -> Ptr SUBNET_ELEMENT_DATA_V4
    -> IO DWORD
foreign import stdcall "dynamic"
    mkAddSubnetElementV4 :: FunPtr AddSubnetElementV4 -> AddSubnetElementV4

-- VOID DHCP_API_FUNCTION DhcpRpcFreeMemory(
--   PVOID BufferPointer
-- );
type RpcFreeMemory = Ptr () -> IO ()
foreign import stdcall "dynamic"
    mkRpcFreeMemory :: FunPtr RpcFreeMemory -> RpcFreeMemory

-- | In an effort to avoid potential compile-time linker errors this package
-- uses runtime dynamic linking. Internally a `DhcpApi` is a dictionary of
-- dynamically bound foreign calls. Most actions require one to be passed.
-- Simply call `loadDhcp` to obtain one.
data DhcpApi = DhcpApi
    { c_DeleteClientInfo :: DeleteClientInfo
    , c_EnumSubnetClientsV4 :: EnumSubnetClientsV4
    , c_EnumSubnetElementsV4 :: EnumSubnetElementsV4
    , c_GetClientInfoV4 :: GetClientInfoV4
    , c_RemoveSubnetElementV4 :: RemoveSubnetElementV4
    , c_AddSubnetElementV4 :: AddSubnetElementV4
    , c_RpcFreeMemory :: RpcFreeMemory
    }

-- | Calling this function performs runtime dynamic linking for every internal
-- foreign call into the Dhcp Server Management Api. It is safe to call this
-- action multiple times. I recommend calling this function once as the part
-- of a process's initialization, and then pass the returned `DhcpApi` to
-- functions that need it.
loadDHCP :: IO DhcpApi
loadDHCP = do
    lib <- loadLibrary "dhcpsapi"
    deleteclient <- getProcAddress lib "DhcpDeleteClientInfo"
    enumClients <- getProcAddress lib "DhcpEnumSubnetClientsV4"
    enumElements <- getProcAddress lib "DhcpEnumSubnetElementsV4"
    getclient <- getProcAddress lib "DhcpGetClientInfoV4"
    remove <- getProcAddress lib "DhcpRemoveSubnetElementV4"
    add <- getProcAddress lib "DhcpAddSubnetElementV4"
    rpcfree <- getProcAddress lib "DhcpRpcFreeMemory"
    return $ DhcpApi (mkDeleteClientInfo . castPtrToFunPtr $ deleteclient)
                     (mkEnumSubnetClientsV4 . castPtrToFunPtr $ enumClients)
                     (mkEnumSubnetElementsV4 . castPtrToFunPtr $ enumElements)
                     (mkGetClientInfoV4 . castPtrToFunPtr $ getclient)
                     (mkRemoveSubnetElementV4 . castPtrToFunPtr $ remove)
                     (mkAddSubnetElementV4 . castPtrToFunPtr $ add)
                     (mkRpcFreeMemory . castPtrToFunPtr $ rpcfree)

-- |Free a block of memory created by DHCP's api.
--  MSDN only mensions this function in DhcpEnumSubnetElementsV5. The original,
--  v4, and v6 variants of the function don't say how memory should be freed.
rpcFreeMemory :: DhcpApi -> Ptr a -> IO ()
rpcFreeMemory api ptr = c_RpcFreeMemory api $ castPtr ptr
