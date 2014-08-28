module Utils where

import Control.Applicative
import Foreign

import System.Win32.Types

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

-- |Peek a string that might be null. Null pointers become Nothing
peekMaybeTString :: LPTSTR -> IO (Maybe String)
peekMaybeTString ptr
  | ptr == nullPtr = return Nothing
  | otherwise      = Just <$> peekTString ptr

-- |Temporarily marshal a Maybe String. A nothing becomes a null pointer.
withMaybeTString :: Maybe String -> (LPTSTR -> IO a) -> IO a
withMaybeTString Nothing    f = f nullPtr
withMaybeTString (Just str) f = withTString str f

note :: e -> Maybe a -> Either e a
note e Nothing = Left e
note _ (Just x) = Right x

fmapL :: (a -> b) -> Either a r -> Either b r
fmapL f (Left x) = Left (f x)
fmapL _ (Right x) = (Right x)
