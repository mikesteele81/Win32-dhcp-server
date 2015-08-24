{-# LANGUAGE CPP #-}

module Import
  ( module X
  -- * Utilities borrowed from 'errors'
  , hush
  , note
  , fmapL
    -- * Text-based string marshallers
  , withTString
  , withTStringLen
  , withMaybeTString
  , peekMaybeTString
    -- * memory cleanup
  , scrubbing
  , scrubbing_
  , scrubWith_
  , scrubWith
  ) where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative as X
#endif
import Data.Char (chr)
import Data.Monoid as X
import Foreign as X
import Foreign.C.Types as X

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Foreign as T
import System.Win32.Error as X
import System.Win32.Error.Foreign as X
import System.Win32.Types as X
    hiding ( ErrCode, failIfNull, failWith, failUnlessSuccess
           , failIfFalse_, failIf, errorWin, withTString, withTStringLen)
import qualified System.Win32.Types as W32

-- |Peek a string that might be null. Null pointers become Nothing
peekMaybeTString :: LPTSTR -> IO (Maybe String)
peekMaybeTString ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekTString ptr

-- |Temporarily marshal a Maybe String. A nothing becomes a null pointer.
withMaybeTString :: Maybe String -> (LPTSTR -> IO a) -> IO a
withMaybeTString Nothing    f = f nullPtr
withMaybeTString (Just str) f = W32.withTString str f

withTStringLen :: Text -> (LPTSTR -> T.I16 -> IO a) -> IO a
withTStringLen text act = T.useAsPtr (T.snoc text (chr 0x0)) $ \ptr len ->
    act (castPtr ptr) len

withTString :: Text -> (LPTSTR -> IO a) -> IO a
withTString text act = withTStringLen text $ \ptr _ -> act ptr

-- |Suppress the 'Left' value of an 'Either'
-- taken from the errors package
hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

-- |taken from the errors package
note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just x) = Right x

-- |taken from the errors package
fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f (Left x) = Left (f x)
fmapL _ (Right x) = (Right x)

-- | Perform an action over a double pointer, and then zero it out. If the
-- double pointer is already zeroed out, do nothing and return 'Nothing'.
scrubbing :: (Ptr a -> IO b) -> Ptr (Ptr a) -> IO (Maybe b)
scrubbing f pptr = do
    ptr <- peek pptr
    if ptr == nullPtr
      then return Nothing
      else do
        ret <- f ptr
        poke pptr nullPtr
        return $ Just ret

-- |Perform a cleanup operation on memory.
scrubbing_ :: (Ptr a -> IO ()) -> Ptr (Ptr a) -> IO ()
scrubbing_ f pptr = do
    _ <- scrubbing f pptr
    return ()

scrubWith :: Ptr (Ptr a) -> (Ptr a -> IO b) -> IO (Maybe b)
scrubWith = flip scrubbing

scrubWith_ :: Ptr (Ptr a) -> (Ptr a -> IO ()) -> IO ()
scrubWith_ pptr f = do
    _ <-scrubWith pptr f
    return ()
