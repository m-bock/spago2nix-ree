module Spago2Nix.Data.SpagoDhall
  ( SpagoDhall
  , codecSpagoDhall
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as Codec.Record

type SpagoDhall
  = { dependencies :: Array String
    }

codecSpagoDhall :: JsonCodec SpagoDhall
codecSpagoDhall =
  Codec.object "SpagoDhall"
    $ Codec.Record.record
        { dependencies: Codec.array Codec.string
        }
