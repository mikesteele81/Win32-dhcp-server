-- | This module was taken, with modifications, from the
-- <http://hackage.haskell.org/package/maccatcher maccatcher> package.
module Data.Mac
    ( Mac ()
    -- ** String conversions
    , readMac
    , showMac
    -- ** Octet conversions
    , fromOctets
    , toOctets
    -- ** Other conversions
    , toWord64
    ) where

import Control.Applicative
import Data.Bits
import Data.List
import Data.Word
import Foreign
import Numeric (showHex)
import Safe

-- |A Mac is a 6-byte unique identifier used in layer-two network addressing.
-- Its `Storable` instance occupies 6 bytes of memory when `poke`d with the
-- first byte occupying the lowest address, and the last byte occupying the
-- highest address.
--
-- A `Show` instance is omitted to avoid confusion. Use `showMac` and
-- `readMac` to convert between `String`s.
newtype Mac =  Mac { unMac :: Word64 }
   deriving (Bounded, Eq, Ord)

instance Storable Mac where
  sizeOf _ =  6
  alignment _ =  1
  peek p =  fromOctets <$> (peek $ castPtr p) <*> peekByteOff p 1
      <*> peekByteOff p 2 <*> peekByteOff p 3 <*> peekByteOff p 4
      <*> peekByteOff p 5
  poke p mac   =  do
    poke (castPtr p) a
    pokeByteOff p 1 b
    pokeByteOff p 2 c
    pokeByteOff p 3 d
    pokeByteOff p 4 e
    pokeByteOff p 5 f
    where
      (a, b, c, d, e, f) = toOctets mac

toWord64 :: Mac -> Word64
toWord64 = unMac

-- |Represent a `Mac` as a `String`. The supplied separator will be placed
-- between each octet in the final output.
--
-- >>> showMac "" $ fromOctets 0xa 0xb 0xc 0xd 0xe 0xf
-- "0a0b0c0d0e0f"
--
-- >>> showMac ":" $ fromOctets 0x11 0x22 0x33 0x44 0x55 0x66
-- "11:22:33:44:55:66"
showMac :: String -> Mac -> String
showMac sep mac = intercalate sep
                . map (\n -> pad 2 $ showHex n "")
                $ [a, b, c, d, e, f]
  where
    (a, b, c, d, e, f) = toOctets mac

pad :: Int -> String -> String
pad n str = replicate zeroes '0' ++ str
  where
    zeroes = max 0 (n - length str)

-- |Parse a `String` value as a `Mac`. The string should not use any
-- separators between octets.
--
-- >>> let Just mac = readMac "000102030405"
-- >>> toOctets mac
-- (0, 1, 2, 3, 4, 5)
readMac :: String -> Maybe Mac
readMac str = case sequence (go str) of
    Just [a, b, c, d, e, f] -> Just $ fromOctets a b c d e f
    _ -> Nothing
   where
    go :: String -> [Maybe Word8]
    -- There should always be 2 characters
    go [_] = [Nothing]
    -- We've reached the end
    go [] = []
    go s = let (b, rest) = splitAt 2 s in readMay ("0x" ++ b) : go rest

-- |A Mac address is 48-bits wide. This function will construct a `Mac` from
-- 6 octets.
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Word8 -> Mac
fromOctets a b c d e f = Mac
      $ (fromIntegral a `shiftL` 40)
    .|. (fromIntegral b `shiftL` 32)
    .|. (fromIntegral c `shiftL` 24)
    .|. (fromIntegral d `shiftL` 16)
    .|. (fromIntegral e `shiftL` 8)
    .|. (fromIntegral f)

-- |Extract each of the 6 octets from a `Mac`.
toOctets :: Mac -> (Word8, Word8, Word8, Word8, Word8, Word8)
toOctets (Mac word) = ( byte 5 word, byte 4 word, byte 3 word
                      , byte 2 word, byte 1 word, byte 0 word)
  where
    byte i w = fromIntegral (w `shiftR` (i * 8))
