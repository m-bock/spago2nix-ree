module Spago2Nix.Data.URI where

import Data.Codec.Argonaut (JsonCodec)
import Partial.Unsafe (unsafeCrashWith)

type URI
  = {}

codec :: JsonCodec URI
codec = unsafeCrashWith "Spago2Nix.Data.URI.codec"
