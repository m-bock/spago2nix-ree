module Spago2Nix.SpagoPackage
  ( SpagoPackage
  , SpagoPackageFields
  , SpagoPackageLock
  ) where

type SpagoPackage
  = SpagoPackageFields ()

type SpagoPackageFields r
  = { dependencies :: Array String
    , version :: String
    , repo :: String
    | r
    }

type SpagoPackageLock
  = SpagoPackageFields
      ( rev :: String
      , nixSha256 :: String
      )
