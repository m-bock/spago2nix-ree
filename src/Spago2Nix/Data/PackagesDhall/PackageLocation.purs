module Spago2Nix.Data.PackagesDhall.PackageLocation
  ( codecPackageLocation
  , PackageLocation(..)
  , PackageRemote
  , PackageLocal
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Codec (decode, encode)
import Data.Codec.Argonaut (JsonCodec, json, prismaticCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Either as Either
import Pathy (AnyFile)
import Pathy as Pathy
import Spago2Nix.Data.Codec.Argonaut.Compat.Pathy.Unsandboxed (codecAnyFile)
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
  = AnyFile

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
  codecAnyFile
    { parser: Pathy.posixParser
    , printer: Pathy.posixPrinter
    }

codecPackageLocation :: JsonCodec PackageLocation
codecPackageLocation = prismaticCodec fromJson toJson json
  where
  fromJson x =
    (decode codecPackageRemote x <#> Remote)
      <|> (decode codecPackageLocal x <#> Local)
      # Either.hush

  toJson = case _ of
    Remote x -> encode codecPackageRemote x
    Local x -> encode codecPackageLocal x
