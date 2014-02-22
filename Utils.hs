module Utils where

import Control.Applicative
import Control.Monad (unless)
import Foreign

import System.Win32.Types

-- |Many DHCP functions return a large object composed of pointers to other
--  objects. When it comes time to clean up memory we need to walk through
--  the structure. This helper function calls an action to get a pointer
--  to free (usually a variant of peek), and then calls the supplied free
--  action on it. Finally, the pointer is zeroed out.
freeptr :: (Ptr b -> IO ()) -> Ptr (Ptr a) -> IO ()
freeptr rpcfree pptr =
    peek pptr >>= \ptr ->
    unless (ptr == nullPtr) $ do
    rpcfree $ castPtr ptr
    poke pptr nullPtr

-- |Peek a string that might be null. Null pointers become Nothing
peekMaybeTString :: LPTSTR -> IO (Maybe String)
peekMaybeTString ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekTString ptr

-- |Temporarily marshal a Maybe String. A nothing becomes a null pointer.
withMaybeTString :: Maybe String -> (LPTSTR -> IO a) -> IO a
withMaybeTString Nothing    f = f nullPtr
withMaybeTString (Just str) f = withTString str f
