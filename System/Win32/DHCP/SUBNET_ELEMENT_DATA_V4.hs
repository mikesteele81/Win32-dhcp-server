{-# LANGUAGE RankNTypes #-}

module System.Win32.DHCP.SUBNET_ELEMENT_DATA_V4
    ( SUBNET_ELEMENT_DATA_V4 (..)
    , SUBNET_ELEMENT_TYPE
    , elementTypeOf
    , subnetElementData
    ) where

import Control.Applicative
import Foreign
import Foreign.C.Types

import System.Win32.DHCP.DhcpStructure
import System.Win32.DHCP.HOST_INFO
import System.Win32.DHCP.IP_CLUSTER
import System.Win32.DHCP.IP_RANGE
import System.Win32.DHCP.Reservation

-- typedef struct _DHCP_SUBNET_ELEMENT_DATA_V4 {
--   DHCP_SUBNET_ELEMENT_TYPE ElementType;
--   union {
--     DHCP_IP_RANGE          *IpRange;
--     DHCP_HOST_INFO         *SecondaryHost;
--     DHCP_IP_RESERVATION_V4 *ReservedIp;
--     DHCP_IP_RANGE          *ExcludeIpRange;
--     DHCP_IP_CLUSTER        *IpUsedCluster;
--   } Element;
-- } DHCP_SUBNET_ELEMENT_DATA_V4, *LPDHCP_SUBNET_ELEMENT_DATA_V4;
data SUBNET_ELEMENT_DATA_V4
    = IpRanges         !IP_RANGE
    | SecondaryHosts   !HOST_INFO
    | ReservedIps      !Reservation
    | ExcludedIpRanges !IP_RANGE
    | IpUsedCluster    !IP_CLUSTER

-- typedef enum _DHCP_SUBNET_ELEMENT_TYPE_V5 {
--   DhcpIpRanges,
--   DhcpSecondaryHosts,
--   DhcpReservedIps,
--   DhcpExcludedIpRanges,
--   DhcpIpRangesDhcpOnly,
--   DhcpIpRangesDhcpBootp,
--   DhcpIpRangesBootpOnly
-- } DHCP_SUBNET_ELEMENT_TYPE, *LPDHCP_SUBNET_ELEMENT_TYPE;
type SUBNET_ELEMENT_TYPE = CInt

subnetElementData :: DhcpStructure SUBNET_ELEMENT_DATA_V4
subnetElementData = DhcpStructure
    { peekDhcp = peekSubnetElementData
    , freeDhcpChildren = freeSubnetElementData
    , withDhcp' = withSubnetElementData'
    , sizeDhcp = 8
    }

peekSubnetElementData :: Ptr SUBNET_ELEMENT_DATA_V4 -> IO SUBNET_ELEMENT_DATA_V4
peekSubnetElementData ptr = do
    elementType <- (peek . castPtr) ptr :: IO SUBNET_ELEMENT_TYPE
    pElement <- peekByteOff ptr 4
    case elementType of
      0 -> IpRanges         <$> peek (castPtr pElement)
      1 -> SecondaryHosts   <$> peekDhcp hostInfo (castPtr pElement)
      2 -> ReservedIps      <$> peekDhcp reservation (castPtr pElement)
      3 -> ExcludedIpRanges <$> peek (castPtr pElement)
      4 -> IpUsedCluster    <$> peek (castPtr pElement)
      _ -> error "Invalid element type found in SUBNET_ELEMENT_DATA_V4."

freeSubnetElementData :: (forall x. Ptr x -> IO ())
    -> Ptr SUBNET_ELEMENT_DATA_V4 -> IO ()
freeSubnetElementData freefunc ptr = do
    elementType <- (peek . castPtr) ptr :: IO SUBNET_ELEMENT_TYPE
    pElement <- peek $ ppElement ptr
    case elementType of
      0 -> return ()
      1 -> freeDhcp hostInfo freefunc $ castPtr pElement
      2 -> freeDhcp reservation freefunc $ castPtr pElement
      3 -> return ()
      4 -> return ()
      _ -> error "Invalid element type found in SUBNET_ELEMENT_DATA_V4."
    poke (ppElement ptr) nullPtr

-- Also used by withSubnetElementDataArray.
withSubnetElementData' :: SUBNET_ELEMENT_DATA_V4 -> Ptr SUBNET_ELEMENT_DATA_V4
    -> IO r -> IO r
withSubnetElementData' elementData ptr f = do
    castPtr ptr `poke` elementTypeOf elementData
    case elementData of
      IpRanges x -> with x $ \pX -> poke (ppElement ptr) pX >> f
      SecondaryHosts x -> withDhcp hostInfo x $ \pX -> poke (ppElement ptr) pX >> f
      ReservedIps x -> withDhcp reservation x $ \pX -> poke (ppElement ptr) pX >> f
      ExcludedIpRanges x -> with x $ \pX -> poke (ppElement ptr) pX >> f
      IpUsedCluster x -> with x $ \pX -> poke (ppElement ptr) pX >> f

ppElement :: Ptr SUBNET_ELEMENT_DATA_V4 -> Ptr a
ppElement ptr = castPtr $ ptr `plusPtr` 4

elementTypeOf :: SUBNET_ELEMENT_DATA_V4 -> SUBNET_ELEMENT_TYPE
elementTypeOf (IpRanges _)         = 0
elementTypeOf (SecondaryHosts _)   = 1
elementTypeOf (ReservedIps _)      = 2
elementTypeOf (ExcludedIpRanges _) = 3
elementTypeOf (IpUsedCluster _)    = 4
