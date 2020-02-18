let sources = import ./nix/sources.nix;

in {
# PKGS
pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.dhall-json,

nodejs ? pkgs.nodejs,

yarn ? pkgs.yarn,

nixfmt ? pkgs.nixfmt,

nix-prefetch-git ? pkgs.nix-prefetch-git,

jq ? pkgs.jq,

# EASY PURESCRIPT
easy-purescript-nix ? import sources.easy-purescript-nix { inherit pkgs; },

spago2nix ? easy-purescript-nix.spago2nix,

purs ? easy-purescript-nix.purs,

# YARN2NIX

yarn2nix ? import sources.yarn2nix { inherit pkgs; },

#

dhall-haskell ? (import sources.dhall-haskell)

}:
with builtins;
let

  buildPackage = import ./build-package.nix { };

  buildProject = {

    spagoPackages ? ./packages-lock.json,

    spagoConfig ? "spago.dhall",

    src ? ./.

    }:
    let

      spagoConfig' = import (pkgs.runCommand "yx" { } ''
        cd ${src};
        echo "./${spagoConfig}" | ${dhall-haskell.dhall-nix}/bin/dhall-to-nix > $out
      '');

      projectPackage = {
        name = spagoConfig'.name;
        dependencies = spagoConfig'.dependencies;
        source = pkgs.runCommand "source" ''
          mkdir $out
          ln -s ${./src} $out/src
        '';
      };
    in pkgs.runCommand "x" { } "touch $out; echo ${spagoConfig'.x}"
    /* buildPackage {
         inherit spagoPackages;
         package = projectPackage;
       }
    */
  ;

in {
  spago2nix = import ./spago2nix-cli.nix { };
  inherit buildProject;
  inherit buildPackage;
}
