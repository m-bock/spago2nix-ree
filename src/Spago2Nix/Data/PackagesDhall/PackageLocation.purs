module Spago2Nix.Data.PackagesDhall.PackageLocation
  ( codecPackageLocation
  , PackageLocation(..)
  , PackageRemote
  , PackageLocal
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Codec (basicCodec, decode, encode)
import Data.Codec.Argonaut (JsonCodec, JsonDecodeError(..))
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe, maybe)
import Pathy (AnyDir)
import Pathy as Pathy
import Spago2Nix.Data.Codec.Argonaut.Compat.Pathy.Unsandboxed (codecAnyDir)
import Spago2Nix.Data.URI (URI)
import Spago2Nix.Data.URI as URI

data PackageLocation
  = Remote PackageRemote
  | Local PackageLocal

type PackageRemote
  = { dependencies :: Array String
    , version :: String
    , repo :: URI
    }

type PackageLocal
  = AnyDir

codecPackageRemote :: JsonCodec PackageRemote
codecPackageRemote =
  Codec.object "PackageRemote"
    $ Codec.Record.record
        { dependencies: Codec.array Codec.string
        , version: Codec.string
        , repo: URI.codec
        }

codecPackageLocal :: JsonCodec PackageLocal
codecPackageLocal =
  codecAnyDir
    { parser: Pathy.posixParser
    , printer: Pathy.posixPrinter
    }

codecPackageLocation :: JsonCodec PackageLocation
codecPackageLocation = jsonPrimCodec "PackageLocation" dec enc
  where
  dec x =
    (decode codecPackageRemote x <#> Remote)
      <|> (decode codecPackageRemote x <#> Remote)
      # Either.hush

  enc = case _ of
    Remote x -> encode codecPackageRemote x
    Local x -> encode codecPackageLocal x

jsonPrimCodec ::
  forall a.
  String ->
  (Json -> Maybe a) ->
  (a -> Json) ->
  JsonCodec a
jsonPrimCodec ty f = basicCodec (maybe (Left (TypeMismatch ty)) pure <<< f)
