module Codec.Compression.BWT
  ( 
  -- * For Pure Code
    encodeBwt
  , decodeBwt
  , encodeMultiBwt
  , decodeMultiBwt
  -- * For use in IO
  , encodeBwtIO
  , decodeBwtIO
  , encodeMultiBwtIO
  , decodeMultiBwtIO
  ) where

import Codec.Compression.Encode -- (encodeBwt, encodeBwtIO)
import Codec.Compression.Decode -- (decodeBwt, decodeBwtIO)


