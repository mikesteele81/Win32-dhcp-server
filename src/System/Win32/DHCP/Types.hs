module System.Win32.DHCP.Types
  ( ClientType (..)
  , DATE_TIME (..)
  , FORCE_FLAG (..)
  , RESUME_HANDLE
  ) where

import Import

-- | Microsoft's DHCP server supports DHCP and BOOTP. Both protocols server
-- similar purposes, but DHCP is more widely used. Lease and reservation
-- records contain a flag field indicating which protocol the record is valid
-- for. In most cases this flag will be `Both`, because that is the default
-- behavior.
data ClientType = Unspecified | Dhcp | Bootp | Both | ReservationFlag | None
    deriving (Eq)

instance Enum ClientType where
  toEnum 0x00 = Unspecified
  toEnum 0x01 = Dhcp
  toEnum 0x02 = Bootp
  toEnum 0x03 = Both
  toEnum 0x04 = ReservationFlag
  toEnum 0x64 = None
  toEnum x    = error $ "invalid ClientType: " ++ show x
  fromEnum Unspecified     = 0x00
  fromEnum Dhcp            = 0x01
  fromEnum Bootp           = 0x02
  fromEnum Both            = 0x03
  fromEnum ReservationFlag = 0x04
  fromEnum None            = 0x64

instance Storable ClientType where
  sizeOf _ = sizeOf (undefined :: BYTE)
  alignment _ = alignment (undefined :: BYTE)
  peek ptr = toEnum . fromIntegral <$> peek (pbyte ptr)
  poke ptr ct = poke (pbyte ptr) . fromIntegral . fromEnum $ ct

pbyte :: Ptr ClientType -> Ptr BYTE
pbyte = castPtr

-- | The number of ticks (100-nanosecond increments) since 12:00 midnight,
-- January 1, 1 C.E. in the Gregorian calendar.
--
-- Microsoft does not provide any functions I know of for converting this
-- value into something more convenient to work with.
--
-- > typedef struct _DATE_TIME {
-- >     DWORD dwLowDateTime;
-- >     DWORD dwHighDateTime;
-- > } DATE_TIME,*PDATE_TIME, *LPDATE_TIME;
data DATE_TIME = DATE_TIME !DWORD !DWORD

instance Storable DATE_TIME where
  sizeOf _ = 8
  alignment _ = 4
  peek ptr = DATE_TIME <$> peekElemOff (pdword ptr) 0
                       <*> peekElemOff (pdword ptr) 1
  poke ptr (DATE_TIME l h) = do
    pokeElemOff (pdword ptr) 0 l
    pokeElemOff (pdword ptr) 1 h

pdword :: Ptr DATE_TIME -> Ptr DWORD
pdword ptr = castPtr ptr

-- typedef enum _DHCP_FORCE_FLAG {
--   DhcpFullForce,
--   DhcpNoForce
-- } DHCP_FORCE_FLAG, *LPDHCP_FORCE_FLAG;
data FORCE_FLAG = FullForce | NoForce
    deriving (Enum)

type RESUME_HANDLE = DWORD
