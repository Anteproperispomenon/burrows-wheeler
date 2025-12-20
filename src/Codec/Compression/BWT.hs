module Codec.Compression.BWT
  ( 
  -- * For Pure Code
    encodeBwt
  , decodeBwt
  -- * For use in IO
  , encodeBwtIO
  , decodeBwtIO
  ) where

import Codec.Compression.Encode (encodeBwt, encodeBwtIO)
import Codec.Compression.Decode (decodeBwt, decodeBwtIO)


