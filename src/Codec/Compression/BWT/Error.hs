{-# OPTIONS_HADDOCK hide #-}

module Codec.Compression.BWT.Error
  ( noMemError
  , inValError
  ) where

import Foreign.C.Error qualified as CErr

import Control.Exception

-- CErr.eNOMEM
-- CErr.eINVAL

noMemError :: String -> IOError
noMemError loc = CErr.errnoToIOError loc CErr.eNOMEM Nothing Nothing

inValError :: String -> IOError
inValError loc = CErr.errnoToIOError loc CErr.eINVAL Nothing Nothing

{-
noMemError :: String -> String -> IOException
noMemError loc dsc = IOError
  { ioe_handle   = Nothing
  , ioe_type     = ResourceExhausted
  , ioe_location = loc
  , ioe_description = dsc
  , ioe_errno    = Just CErr.eNOMEM
  , ioe_filename = Nothing
  }
-}

