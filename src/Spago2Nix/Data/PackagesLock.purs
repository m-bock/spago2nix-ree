module Spago2Nix.Data.PackagesLock
  ( module Export
  , PackagesLock
  , codecPackagesLock
  ) where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Extra as Codec.Extra
import Data.Map (Map)
import Spago2Nix.Data.PackagesLock.PackageLocation as PackagesLock.PackageLocation
import Spago2Nix.Data.PackagesLock.PackageLocation (PackageLocation(..)) as Export

type PackagesLock
  = Map String PackagesLock.PackageLocation.PackageLocation

codecPackagesLock ∷ JsonCodec PackagesLock
codecPackagesLock = Codec.Extra.codecMap PackagesLock.PackageLocation.codecPackageLocation
