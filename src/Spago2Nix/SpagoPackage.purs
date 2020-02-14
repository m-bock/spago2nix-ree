module Spago2Nix.SpagoPackage (SpagoPackage, SpagoPackageEnriched, SpagoPackageSpec, toNix) where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))
import NixAST (NixExpr)
import NixAST as Nix

type SpagoPackage
  = SpagoPackageSpec ()

type SpagoPackageEnriched
  = SpagoPackageSpec ( nixSHA :: String )

type SpagoPackageSpec r
  = { dependencies :: Array String
    , version :: String
    , repo :: String
    | r
    }

toNix :: forall f. Foldable f => f (String /\ SpagoPackageEnriched) -> NixExpr
toNix spagoPackages =
  Nix.AttrSet
    ( spagoPackages
        # Array.fromFoldable
        <#> (\(key /\ value) -> key /\ spagoPackageEnriched_toNix value)
    )

spagoPackageEnriched_toNix :: SpagoPackageEnriched -> NixExpr
spagoPackageEnriched_toNix { dependencies, version, repo, nixSHA } =
  Nix.AttrSet
    [ "dependencies" /\ Nix.List (dependencies <#> Nix.String)
    , "version" /\ Nix.String version
    , "source"
        /\ identity
            ( Nix.App (Nix.DotAccess [ "pkgs", "fetchgit" ])
                [ Nix.AttrSet
                    [ "url" /\ Nix.String repo
                    , "sha256" /\ Nix.String nixSHA
                    , "rev" /\ Nix.String version
                    ]
                ]
            )
    ]
