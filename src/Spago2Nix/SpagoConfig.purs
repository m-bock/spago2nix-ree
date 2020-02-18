module Spago2Nix.SpagoConfig
  ( SpagoConfig
  , codec_SpagoConfig
  , SpagoPackage
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Map (Map)
import Spago2Nix.Common (codec_map)

type SpagoConfig
  = { name :: String
    , dependencies :: Array String
    , packages :: Map String SpagoPackage
    , sources :: Array String
    }

type SpagoPackage
  = { dependencies :: Array String
    , version :: String
    , repo :: String
    }

codec_SpagoPackage ∷ JsonCodec SpagoPackage
codec_SpagoPackage =
  Codec.object "SpagoPackage"
    $ Codec.Record.record
        { dependencies: Codec.array Codec.string
        , version: Codec.string
        , repo: Codec.string
        }

codec_SpagoConfig ∷ JsonCodec SpagoConfig
codec_SpagoConfig =
  Codec.object "SpagoConfig"
    $ Codec.Record.record
        { name: Codec.string
        , dependencies: Codec.array Codec.string
        , packages: codec_map codec_SpagoPackage
        , sources: Codec.array Codec.string
        }
