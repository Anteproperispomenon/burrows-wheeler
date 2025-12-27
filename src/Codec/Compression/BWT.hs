module Codec.Compression.BWT
  ( 
  -- * For Pure Code
    encodeBwt
  , decodeBwt
  , encodeBwtPtr
  , decodeBwtPtr
  , encodeMultiBwt
  , decodeMultiBwt
  -- * For use in IO
  , encodeBwtIO
  , decodeBwtIO
  , encodeBwtPtr
  , decodeBwtPtr
  , encodeMultiBwtIO
  , decodeMultiBwtIO
  ) where

import Codec.Compression.Encode -- (encodeBwt, encodeBwtIO)
import Codec.Compression.Decode -- (decodeBwt, decodeBwtIO)


