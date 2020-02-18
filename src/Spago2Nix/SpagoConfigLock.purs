module Spago2Nix.SpagoConfigLock
  ( SpagoConfigLock
  , codec_SpagoConfigLock
  , SpagoPackageLock
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Map (Map)
import Spago2Nix.Common (codec_map)

type SpagoConfigLock
  = { name :: String
    , dependencies :: Array String
    , packages :: Map String SpagoPackageLock
    , sources :: Array String
    }

type SpagoPackageLock
  = { name :: String
    , dependencies :: Array String
    , version :: String
    , repo :: String
    , rev :: String
    , nixSha256 :: String
    }

codec_SpagoPackageLock ∷ JsonCodec SpagoPackageLock
codec_SpagoPackageLock =
  Codec.object "SpagoPackageLock"
    $ Codec.Record.record
        { name: Codec.string
        , dependencies: Codec.array Codec.string
        , version: Codec.string
        , repo: Codec.string
        , rev: Codec.string
        , nixSha256: Codec.string
        }

codec_SpagoConfigLock ∷ JsonCodec SpagoConfigLock
codec_SpagoConfigLock =
  Codec.object "SpagoConfigLock"
    $ Codec.Record.record
        { name: Codec.string
        , dependencies: Codec.array Codec.string
        , packages: codec_map codec_SpagoPackageLock
        , sources: Codec.array Codec.string
        }
