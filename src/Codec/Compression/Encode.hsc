{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Codec.Compression.Encode
  ( encodeBwtIO
  , encodeBwt
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

import System.IO.Unsafe

#include "encode.h"

foreign import ccall "encode.h do_bwt"
  -- c_do_bwt :: CStringLen -> CStringLen -> CInt -> IO CInt
  c_do_bwt :: CString -> CString -> CInt -> IO CInt

encodeBwtIO :: BS.ByteString -> IO BS.ByteString
encodeBwtIO bstr = do
  let len = BS.length bstr
  ptr <- mallocBytes (4+len)
  let ptrLen = (ptr, 4+len)
  rslt <- BSU.unsafeUseAsCString bstr (\cstr -> c_do_bwt cstr ptr (CInt $ fromIntegral len))
  BSU.unsafePackMallocCStringLen ptrLen

encodeBwt :: BS.ByteString -> BS.ByteString
encodeBwt bstr = unsafePerformIO $ encodeBwtIO bstr
{-# NOINLINE encodeBwt #-}
