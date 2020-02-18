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

}:
with builtins;
let

  buildPackage = import ./build-package.nix { };

  buildProject = {

    src ? ./.,

    spagoLock ? src + "/spago-lock.json"

    }:
    let

      spagoLock' = fromJSON (readFile spagoLock);

      projectPackage = {
        name = spagoLock'.name;
        dependencies = spagoLock'.dependencies;
        version = "v0.0.0";
        source = pkgs.runCommand "source" { } ''
          mkdir $out
          ln -s ${src}/src $out/src
        '';
      };

      spagoPackages = mapAttrs (_: package:
        let
          source = pkgs.fetchgit {
            sha256 = package.nixSha256;
            url = package.repo;
            inherit (package) rev;
          };

        in package // { inherit source; }

      ) spagoLock'.packages;

    in buildPackage {
      inherit spagoPackages;
      package = projectPackage;
    };

in {
  spago2nix = import ./spago2nix-cli.nix { };
  inherit buildProject;
}
