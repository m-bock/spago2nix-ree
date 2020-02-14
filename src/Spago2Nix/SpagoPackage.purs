module Spago2Nix.SpagoPackage (SpagoPackage, SpagoPackageEnriched, SpagoPackageSpec, toNix) where

import Prelude
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\), type (/\))
import NixAST (NixExpr)
import NixAST as Nix
import Spago2Nix.Common (NixPrefetchGitResult)

type SpagoPackage
  = SpagoPackageSpec ()

type SpagoPackageEnriched
  = SpagoPackageSpec ( git :: NixPrefetchGitResult )

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
        <#> (\(key /\ value) -> key /\ spagoPackageEnriched_toNix (key /\ value))
    )

spagoPackageEnriched_toNix :: String /\ SpagoPackageEnriched -> NixExpr
spagoPackageEnriched_toNix (name /\ { dependencies, version, repo, git }) =
  Nix.AttrSet
    [ "name" /\ Nix.String name
    , "dependencies" /\ Nix.List (dependencies <#> Nix.String)
    , "version" /\ Nix.String version
    , "git"
        /\ Nix.AttrSet
            [ "url" /\ Nix.String git.url
            , "sha256" /\ Nix.String git.sha256
            , "rev" /\ Nix.String git.rev
            ]
    ]
