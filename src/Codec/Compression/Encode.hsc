{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Codec.Compression.Encode
  ( encodeBwtIO
  , encodeBwtPtrIO
  , encodeMultiBwtIO
  , encodeBwt
  , encodeBwtPtr
  , encodeMultiBwt
  ) where

import Control.Exception (ioError)

import Control.Monad

import Codec.Compression.BWT.Error

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

import Foreign.Storable

#include "encode.h"

foreign import ccall "encode.h do_bwt"
  -- c_do_bwt :: CStringLen -> CStringLen -> CInt -> IO CInt
  c_do_bwt :: CString -> CString -> CInt -> IO CInt

foreign import ccall "encode.h do_bwt_no_hdr"
  -- c_do_bwt :: CStringLen -> CStringLen -> CInt -> IO CInt
  c_do_bwt_no_hdr :: CString -> CString -> CInt -> IO CInt

-- int do_bwt_alt (const unsigned char *inputArray, unsigned char *outputArray, int *workArray, int sz) ;
foreign import ccall "encode.h do_bwt_alt"
  c_do_bwt_alt :: CString -> CString -> Ptr CInt -> CInt -> IO CInt

encodeBwtIO :: BS.ByteString -> IO BS.ByteString
encodeBwtIO bstr = do
  let len = BS.length bstr
  -- when (len > 0x7FFFFFFF) (fail ())
  ptr <- mallocBytes (4+len)
  let ptrLen = (ptr, 4+len)
  rslt <- BSU.unsafeUseAsCString bstr (\cstr -> c_do_bwt cstr ptr (CInt $ fromIntegral len))
  if | (rslt == (-1)) -> ioError (inValError "encodeBwtIO")
     | (rslt <  (-1)) -> ioError (noMemError "encodeBwtIO")
     | otherwise      -> return ()
  BSU.unsafePackMallocCStringLen ptrLen

encodeBwtPtrIO :: BS.ByteString -> IO (Word32, BS.ByteString)
encodeBwtPtrIO bstr = do
  let len = BS.length bstr
  ptr <- mallocBytes len
  let ptrLen = (ptr, len)
  rslt <- BSU.unsafeUseAsCString bstr (\cstr -> c_do_bwt_no_hdr cstr ptr (CInt $ fromIntegral len))
  if | (rslt == (-1)) -> ioError (inValError "encodeBwtPtrIO")
     | (rslt <  (-1)) -> ioError (noMemError "encodeBwtPtrIO")
     | otherwise      -> return ()
  (fromIntegral rslt, ) <$> BSU.unsafePackMallocCStringLen ptrLen


encodeBwt :: BS.ByteString -> BS.ByteString
encodeBwt bstr = unsafePerformIO $ encodeBwtIO bstr
{-# NOINLINE encodeBwt #-}

encodeBwtPtr :: BS.ByteString -> (Word32, BS.ByteString)
encodeBwtPtr bstr = unsafePerformIO $ encodeBwtPtrIO bstr
{-# NOINLINE encodeBwtPtr #-}

-- | Encode a number of `BS.ByteString`s with BWT in
--   a row. Unlike @mapM encodeBwtIO@, this only allocates
--   one auxilliary array, instead of one for each ByteString.
encodeMultiBwtIO :: [BS.ByteString] -> IO [BS.ByteString]
encodeMultiBwtIO [] = return []
encodeMultiBwtIO [bstr] = (:[]) <$> encodeBwtIO bstr
encodeMultiBwtIO bstrs = do
  let lens   = map BS.length bstrs
      maxLen = maximum (0:lens)
  workPtr <- mallocBytes (maxLen * (sizeOf (CInt 5)))
  outBstrs <- forM bstrs $ \thisBstr -> do
    if BS.null thisBstr
      then return BS.empty
      else do
        let len = BS.length thisBstr
        outPtr <- mallocBytes (4 + len)
        rslt <- BSU.unsafeUseAsCString thisBstr (\cstr -> c_do_bwt_alt cstr outPtr workPtr (fromIntegral len))
        if | (rslt == (-1)) -> ioError (inValError "encodeMultiBwtIO")
           | (rslt <  (-1)) -> ioError (noMemError "encodeMultiBwtIO")
           | otherwise      -> return ()
        BSU.unsafePackMallocCStringLen (outPtr, 4 + len)
  free workPtr
  return outBstrs

encodeMultiBwt :: [BS.ByteString] -> [BS.ByteString]
encodeMultiBwt bstrs = unsafePerformIO (encodeMultiBwtIO bstrs)
{-# NOINLINE encodeMultiBwt #-}
