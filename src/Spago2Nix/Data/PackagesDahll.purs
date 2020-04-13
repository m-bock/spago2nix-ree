module Spago2nix.Data.PackagesDhall where

import Data.Codec.Argonaut (JsonCodec)
import Data.Codec.Argonaut.Extra as Codec.Extra
import Data.Map (Map)
import Spago2Nix.Data.PackageLocation (PackageLocation, codecPackageLocation)

type PackagesDhall
  = Map String PackageLocation

codecPackagesDhall ∷ JsonCodec PackagesDhall
codecPackagesDhall = Codec.Extra.codecMap codecPackageLocation
