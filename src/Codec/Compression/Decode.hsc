{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

module Codec.Compression.Decode
  ( decodeBwtIO
  , decodeBwt
  ) where

import Data.ByteString qualified as BS

import Data.ByteString.Unsafe qualified as BSU

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

decodeBwtIO :: BS.ByteString -> IO BS.ByteString
decodeBwtIO bstr = do
  if (BS.length bstr <= 4)
    then return (BS.empty)
    else do
      let bstr' = BS.drop 4 bstr
          len   = BS.length bstr'
          pidx  = fromIntegral $ makeWord32LE (bstr ! 0) (bstr ! 1) (bstr ! 2) (bstr ! 3)
      ptr <- mallocBytes len
      -- let ptrLen = (ptr, len)
      rslt <- BSU.unsafeUseAsCString bstr' (\cstr -> c_undo_bwt cstr ptr (fromIntegral len) pidx)
      BSU.unsafePackMallocCStringLen (ptr, len)


makeWord32LE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
makeWord32LE (fi -> b1) (fi -> b2) (fi -> b3) (fi -> b4)
  = b1 .|. (b2 `shiftL` 8) .|. (b3 `shiftL` 16) .|. (b4 `shiftL` 24)

fi :: Word8 -> Word32
fi = fromIntegral

(!) :: HasCallStack => BS.ByteString -> Int -> Word8
(!) = BS.index

decodeBwt :: BS.ByteString -> BS.ByteString
decodeBwt bstr = unsafePerformIO $ decodeBwtIO bstr
{-# NOINLINE decodeBwt #-}

