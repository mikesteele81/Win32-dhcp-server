{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module was taken, with modifications, from the
-- <http://hackage.haskell.org/package/maccatcher maccatcher> package.
module Data.Ip
    ( Ip ()
    -- * String conversions
    , readIp
    , showIp
    -- * Octet conversions
    , fromOctets
    , toOctets
    -- * Other conversions
    , toWord32
    ) where

import Control.Monad (unless)
import Data.Bits
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Read
import Data.Word

import Import

-- |An Ip can be used as an IP address or subnet address.
--
-- A `Show` instance is omitted to avoid confusion. Use `showIp` and `readIp`
-- to convert between `String`s.
newtype Ip = Ip {unIp :: Word32}
   deriving (Eq, Ord, Bounded, Storable)

-- |Represent an `Ip` as a `String`.
--
-- >>> showIp $ fromOctets 192 168 1 100
-- "192.168.1.100"
showIp :: Ip -> Text
showIp ip = T.intercalate "." . map (T.pack . show) $ [a, b, c, d]
  where (a, b, c, d) = toOctets ip

-- |Parse a `Text` value as an `Ip`. The string should be of the form
-- "X.X.X.X" where each 'X' is a decimal value between 0 and 255 inclusive.
--
-- >>> let Right ip = readIp "192.168.1.100"
-- >>> toOctets ip
-- (192, 168, 1, 100)
readIp :: Text -> Either String Ip
readIp s = fmapL (\e -> "Error parsing IP address: " ++ e) $ do
   (a, s2) <- decimal s
   (b, s3) <- dot s2 >>= decimal
   (c, s4) <- dot s3 >>= decimal
   (d, s5) <- dot s4 >>= decimal
   unless (s5 == "") $ Left "exactly 4 octets were expected."
   -- Confirm that all digits are in range
   fromOctets <$> digit a <*> digit b <*> digit c <*> digit d
  where
    dot = note "Expected '.' character." . T.stripPrefix "."
    digit :: Int -> Either String Word8
    digit x | x < 0 || x > 255 = Left "digit out of range."
            | otherwise = Right $ fromIntegral x

-- |An IP address is 32-bits wide. This function will construct an `Ip` from
-- 4 octets.
fromOctets :: Word8 -> Word8 -> Word8 -> Word8 -> Ip
fromOctets a b c d = Ip
      $ (fromIntegral a `shiftL` 24)
    .|. (fromIntegral b `shiftL` 16)
    .|. (fromIntegral c `shiftL` 8)
    .|. (fromIntegral d)

-- |Extract each of the 4 octets from an `Ip`.
toOctets :: Ip -> (Word8, Word8, Word8, Word8)
toOctets (Ip word) = (byte 3 word, byte 2 word, byte 1 word, byte 0 word)
  where
    byte i w = fromIntegral (w `shiftR` (i * 8))

toWord32 :: Ip -> Word32
toWord32 = unIp
