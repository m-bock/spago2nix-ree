module Spago2Nix.Data.PackagesDhall
  ( module Export
  , PackagesDhall
  , codecPackagesDhall
  ) where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Extra as Codec.Extra
import Data.Map (Map)
import Spago2Nix.Data.PackagesDhall.PackageLocation as PackagesDhall.PackageLocation
import Spago2Nix.Data.PackagesDhall.PackageLocation (PackageLocation(..)) as Export

type PackagesDhall
  = Map String PackagesDhall.PackageLocation.PackageLocation

codecPackagesDhall ∷ JsonCodec PackagesDhall
codecPackagesDhall = Codec.Extra.codecMap PackagesDhall.PackageLocation.codecPackageLocation
