module Spago2Nix.Data.PackagesLock
  ( PackagesLock
  , PackageLock
  , codecPackagesLock
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Extra (codecMap)
import Data.Codec.Argonaut.Record as Codec.Record
import Data.Map (Map)
import Spago2Nix.Data.PackageLocation (PackageLocation, codecPackageLocation)

type PackagesLock
  = Map String PackageLock

type PackageLock
  = { name :: String
    , dependencies :: Array String
    , version :: String
    , location :: PackageLocation
    }

codecPackageLock ∷ JsonCodec PackageLock
codecPackageLock =
  Codec.object "PackageLock"
    $ Codec.Record.record
        { name: Codec.string
        , dependencies: Codec.array Codec.string
        , version: Codec.string
        , location: codecPackageLocation
        }

codecPackagesLock ∷ JsonCodec PackagesLock
codecPackagesLock = codecMap codecPackageLock
