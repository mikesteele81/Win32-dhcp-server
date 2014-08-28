{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module System.Win32.DHCP.DhcpStructure where

import Control.Applicative
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr
import Foreign.Storable

import Utils

-- |Function dictionary for objects used with the DHCP api.
--  * Ability to peek from a pointer to that object.
--  * Ability to properly free an object using Win32's rpcFreeMemory
--    created by the DHCP api.
--  * Ability to use the with* metaphor to temporarily poke an
--    object into C memory, call a continuation on it, and then
--    free the memory from Haskell's heap.
--
-- Extra features made possible by the typeclass
--  * Ability to turn any Storable instance into a DhpcStructure instance
--    by wrapping it into a StorableDhcpStructure.
--  * Ability to peek an array of DHCP structures into a list.
--  * Ability to poke a list of objects into contiguous memory, then
--    call a continuation on that block of memory.
data DhcpStructure a = DhcpStructure
    { peekDhcp  :: Ptr a -> IO a
    -- |Cleaning up memory is the responsibility of the client of this
    -- library. Most out parameters return compound structures which need to
    -- be recursively navigated, freeing all children, before freeing the main
    -- pointer.
    --
    -- This function only frees child objects without freeing the pointer
    -- itself. It's necessary because some structures contained inline
    -- structures instead of the usual pointer. A separate 'freeDhcp' function
    -- will call this one before freeing its supplied pointer.
    , freeDhcpChildren  :: (forall x. Ptr x -> IO ()) -> Ptr a -> IO ()
    -- |Like `withDhcp`, but without any allocation or cleanup of memory.
    -- The continuation is not automatically given a pointer because
    -- the caller should already have it.
    , withDhcp' :: forall r. a -> Ptr a -> IO r -> IO r
    , sizeDhcp :: Int
    }

freeDhcp :: DhcpStructure a -> (forall x. Ptr x -> IO ()) -> Ptr a -> IO ()
freeDhcp dict free ptr = do
    freeDhcpChildren dict free ptr
    free ptr

-- |Allocate memory for a structure, poke it into memory, apply
-- a function, and then clean up the memory.
withDhcp :: DhcpStructure a -> a -> (Ptr a -> IO r) -> IO r
withDhcp dict a f = allocaBytes (sizeDhcp dict) $ \ptr ->
    withDhcp' dict a ptr $ f ptr

-- |Convert a DhcpStructure so that it can be used with a newtype
-- wrapper.
newtypeDhcpStructure :: (a -> nt) -> (nt -> a)
    -> DhcpStructure a -> DhcpStructure nt
newtypeDhcpStructure wrap unwrap dict =
    DhcpStructure peekNt freeNt withNt' (sizeDhcp dict)
  where
    peekNt ptr = wrap <$> peekDhcp dict (castPtr ptr)
    freeNt :: (forall x. Ptr x -> IO ()) -> Ptr a -> IO ()
    freeNt f ptr = freeDhcpChildren dict f (castPtr ptr)
    withNt' a ptr f = withDhcp' dict (unwrap a) (castPtr ptr) f

-- |This is used in cases like 'CLIENT_UID' where we want to treat it like
-- a 'LengthArray' but individual elements of the array are simple values.
-- We reuse the 'Storable' instances peek and poke functions.
storableDhcpStructure :: forall a. (Storable a) => DhcpStructure a
storableDhcpStructure = DhcpStructure
    { peekDhcp  = peek
    , freeDhcpChildren  = \freeFunc ptr -> freeFunc ptr
    , withDhcp' = withStorable'
    , sizeDhcp  = sizeOf (undefined :: a)
    }
  where
    withStorable' x ptr f = poke ptr x >> f

data DhcpArray a = DhcpArray
    { peekDhcpArray  :: Int -> Ptr a -> IO [a]
    , freeDhcpArray  :: (forall x. Ptr x -> IO ()) -> Int -> Ptr a -> IO ()
    , withDhcpArray' :: forall r. [a] -> Ptr a -> IO r -> IO r
    , dhcpStructure   :: DhcpStructure a
    }

-- |Allocate enough contiguous memory for each element. Recursively
-- free all memory once the supplied function returns.
-- The continuation receives a length argument. This is because
-- the length must be calculated in the course of execution, and will
-- likely be needed again by the caller.
withDhcpArray ::  DhcpArray a -> [a] -> (Int -> Ptr a -> IO r) -> IO r
withDhcpArray dict elems f =
    allocaBytes (stride * size) $ \ptr ->
    -- `f` is meant to be called on the array as a whole; not individual elements.
    -- It's supplied its pointer here because we want it called on position 0.
    withDhcpArray' dict elems ptr $ f size ptr
  where
    size = length elems
    stride = sizeDhcp . dhcpStructure $ dict

-- |This dictionary is a default set to "base" other versions on.
-- Scanning through the buffer happens dhcpSize bytes at a time. Memory
-- is freed by calling the freeing function on every element in the buffer.
baseDhcpArray :: DhcpStructure a -> DhcpArray a
baseDhcpArray s = DhcpArray
    { peekDhcpArray = basePeekArray s
    , freeDhcpArray = baseFreeArray s
    , withDhcpArray' = baseWithArray' s
    , dhcpStructure = s
    }

-- |This differs from `baseDhcpArray` in that the entire buffer
-- is freed with a single call to the freeing function.
basicDhcpArray :: DhcpStructure a -> DhcpArray a
basicDhcpArray dict = (baseDhcpArray dict)
    { freeDhcpArray = basicFreeArray dict
    }

ptrDhcpArray :: DhcpStructure a -> DhcpArray a
ptrDhcpArray dict = (baseDhcpArray dict)
    { peekDhcpArray = ptrPeekArray dict
    , freeDhcpArray = ptrFreeArray dict
    , withDhcpArray' = ptrWithArray' dict
    }

basePeekArray :: DhcpStructure a -> Int -> Ptr a -> IO [a]
basePeekArray dict numElements ptr
  | numElements <= 0 = return []
  | otherwise = f (numElements - 1) []
  where
    f 0 acc = do
        e <- peekDhcp dict ptr
        return (e:acc)
    f n acc = do
        e <- peekDhcp dict (ptr `plusPtr` (sizeDhcp dict * n))
        f (n - 1) (e:acc)

-- |Elements are arranged end to end in a buffer. The buffer is freed
-- at once after each element's children are freed.
baseFreeArray :: DhcpStructure a
    -> (forall x. Ptr x -> IO ()) -> Int -> Ptr a -> IO ()
baseFreeArray dict freefunc len ptr = do
    f (len - 1)
    freefunc ptr
  where
    f 0 = freeDhcpChildren dict freefunc ptr
    f n = do
        freeDhcpChildren dict freefunc $ ptr `plusPtr` (n * sizeDhcp dict)
        f (n - 1)

baseWithArray' :: DhcpStructure a -> [a] -> Ptr a -> IO r -> IO r
baseWithArray' _    []     _   f = f
baseWithArray' dict (e:es) ptr f =
    -- We're not concerned with the individual element.
    withDhcp' dict e ptr
    $ baseWithArray' dict es (ptr `plusPtr` sizeDhcp dict) f

basicFreeArray :: DhcpStructure a -> (forall x. Ptr x -> IO ())
    -> Int -> Ptr a -> IO ()
basicFreeArray dict freefunc _ ptr = freeDhcp dict freefunc ptr

ptrPeekArray :: DhcpStructure a -> Int -> Ptr a -> IO [a]
ptrPeekArray dict numElements ptr
  | numElements <= 0 = return []
  | otherwise = f (numElements - 1) []
  where
    --Each element is a pointer to the real data
    pptr = castPtr ptr
    f 0 acc = do
        e <- peek pptr >>= peekDhcp dict
        return (e:acc)
    f n acc = do
        pe <- peek $ pptr `plusPtr` (sizeOf nullPtr * n)
        e <- peekDhcp dict pe
        f (n - 1) (e:acc)

ptrFreeArray :: DhcpStructure a
    -> (forall x. Ptr x -> IO ()) -> Int -> Ptr a -> IO ()
ptrFreeArray dict freefunc len ptr = do
    -- Scan through and free each element
    f (len - 1)
    freefunc ptr
  where
    --Each element is a pointer to the real data
    pptr0 = castPtr ptr
    f 0 = freeDhcp dict freefunc `scrubbing_` pptr0
    f n = do
        freeDhcp dict freefunc `scrubbing_` plusPtr pptr0 (sizeOf pptr0 * n)
        f (n - 1)

ptrWithArray' :: DhcpStructure a -> [a] -> Ptr a -> IO r -> IO r
ptrWithArray' _    []     _   f = f
ptrWithArray' dict (e:es) ptr f =
    -- We're not concerned with the individual element.
    withDhcp dict e $ \pe -> do
    poke pptr pe
    ptrWithArray' dict es (ptr `plusPtr` sizeOf nullPtr) f
  where
    --Each element is a pointer to the real data
    pptr = castPtr ptr
