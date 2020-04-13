module Spago2Nix.Data.PackageLocation
  ( codecPackageLocation
  , PackageLocation
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Codec.Argonaut.Variant as Codec.Variant
import Data.Either (Either(..))
import Data.Profunctor (dimap)
import Data.Variant (SProxy(..))
import Data.Variant as Variant
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

codecPackageLocation ∷ JsonCodec PackageLocation
codecPackageLocation =
  dimap toVariant fromVariant
    ( Codec.Variant.variantMatch
        { remote: Right codecPackageRemote
        , local: Right codecPackageLocal
        }
    )
  where
  toVariant = case _ of
    Remote x -> Variant.inj (SProxy :: _ "remote") x
    Local x -> Variant.inj (SProxy :: _ "local") x

  fromVariant =
    Variant.match
      { remote: Remote
      , local: Local
      }
