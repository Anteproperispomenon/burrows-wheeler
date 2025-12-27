{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Compression.Decode
  ( decodeBwtIO
  , decodeBwtPtrIO
  , decodeMultiBwtIO
  , decodeBwt
  , decodeBwtPtr
  , decodeMultiBwt
  ) where

import Control.Monad

import Data.ByteString qualified as BS

import Data.ByteString.Unsafe qualified as BSU

import Codec.Compression.BWT.Error

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.C.ConstPtr

import Foreign.Ptr
import Foreign.ForeignPtr

import Foreign.Marshal.Alloc

import Data.Bits
import Data.Word

import GHC.Stack (HasCallStack)

import System.IO.Unsafe

#include "encode.h"

foreign import ccall "decode.h undo_bwt"
  -- c_undo_bwt :: CStringLen -> CStringLen -> CInt -> CInt -> IO CInt
  c_undo_bwt :: CString -> CString -> CInt -> CInt -> IO CInt

-- int do_bwt_alt (const unsigned char *inputArray, unsigned char *outputArray, int *workArray, int sz) ;
foreign import ccall "decode.h undo_bwt_alt"
  c_undo_bwt_alt :: CString -> CString -> Ptr CInt -> CInt -> CInt -> IO CInt


decodeBwtIO :: BS.ByteString -> IO BS.ByteString
decodeBwtIO bstr = do
  if (BS.length bstr <= 4)
    then return (BS.empty)
    else do
      let bstr' = BS.drop 4 bstr
          len   = BS.length bstr'
          pidx0 = makeWord32LE (bstr ! 0) (bstr ! 1) (bstr ! 2) (bstr ! 3)
          pidx  = fromIntegral pidx0
      
      when (fromIntegral pidx /= pidx0) (fail "decodeBwtIO: Data pointer can't safely be converted to a CInt.")
      ptr <- mallocBytes len
      -- let ptrLen = (ptr, len)
      rslt <- BSU.unsafeUseAsCString bstr' (\cstr -> c_undo_bwt cstr ptr (fromIntegral len) pidx)
      if | (rslt == (-1)) -> ioError (inValError "decodeBwtIO")
         | (rslt <  (-1)) -> ioError (noMemError "decodeBwtIO")
         | otherwise      -> return ()
      BSU.unsafePackMallocCStringLen (ptr, len)

-- | Instead of storing the ptr to the 
--   start of data in the `BS.ByteString`,
--   this instead accept it as a separate
--   parameter.
decodeBwtPtrIO :: Word32 -> BS.ByteString -> IO BS.ByteString
decodeBwtPtrIO wd bstr = do
  let len   = BS.length bstr
      pidx  = fromIntegral wd
  -- Extraordinarily unlikey, but just to be safe.
  -- to be clear, 2^31 - 1 ~= 2GiB.
  when (fromIntegral pidx /= wd) (fail "decodeBwtPrtIO: Data pointer can't safely be converted to a CInt.")
  ptr <- mallocBytes len
  -- let ptrLen = (ptr, len)
  rslt <- BSU.unsafeUseAsCString bstr (\cstr -> c_undo_bwt cstr ptr (fromIntegral len) pidx)
  if | (rslt == (-1)) -> ioError (inValError "decodeBwtIO")
     | (rslt <  (-1)) -> ioError (noMemError "decodeBwtIO")
     | otherwise      -> return ()
  BSU.unsafePackMallocCStringLen (ptr, len)

makeWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
makeWord32LE (fi -> b1) (fi -> b2) (fi -> b3) (fi -> b4)
  = b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)

fi :: Word8 -> Word32
fi = fromIntegral

(!) :: HasCallStack => BS.ByteString -> Int -> Word8
(!) = BS.index

decodeBwt :: BS.ByteString -> BS.ByteString
decodeBwt bstr = unsafePerformIO (decodeBwtIO bstr)
{-# NOINLINE decodeBwt #-}

decodeBwtPtr :: Word32 -> BS.ByteString -> BS.ByteString
decodeBwtPtr wd bstr = unsafePerformIO (decodeBwtPtrIO wd bstr)
{-# NOINLINE decodeBwtPtr #-}

-- | Decode a number of `BS.ByteString`s with BWT in
--   a row. Unlike @mapM decodeBwtIO@, this only allocates
--   one auxilliary array, instead of one for each ByteString.
decodeMultiBwtIO :: [BS.ByteString] -> IO [BS.ByteString]
decodeMultiBwtIO [] = return []
decodeMultiBwtIO [bstr] = (:[]) <$> decodeBwtIO bstr
decodeMultiBwtIO bstrs = do
  let lens   = map BS.length bstrs
      maxLen = (maximum (0:lens))
  workPtr <- mallocBytes (maxLen * (sizeOf (CInt 5)))
  outBstrs <- forM bstrs $ \thisBstr -> do
    if (BS.length thisBstr <= 4)
      then return (BS.empty)
      else do
        let bstr' = BS.drop 4 thisBstr
            len   = BS.length bstr'
            pidx  = fromIntegral $ makeWord32LE (thisBstr ! 0) (thisBstr ! 1) (thisBstr ! 2) (thisBstr ! 3)
        outPtr <- mallocBytes len
        rslt <- BSU.unsafeUseAsCString bstr' (\cstr -> c_undo_bwt_alt cstr outPtr workPtr (fromIntegral len) pidx)
        if | (rslt == (-1)) -> ioError (inValError "decodeMultiBwtIO")
           | (rslt <  (-1)) -> ioError (noMemError "decodeMultiBwtIO")
           | otherwise      -> return ()
        BSU.unsafePackMallocCStringLen (outPtr, len)
  free workPtr
  return outBstrs


decodeMultiBwt :: [BS.ByteString] -> [BS.ByteString]
decodeMultiBwt bstrs = unsafePerformIO (decodeMultiBwtIO bstrs)
{-# NOINLINE decodeMultiBwt #-}

