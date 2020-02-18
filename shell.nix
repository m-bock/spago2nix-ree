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

nix ? pkgs.nix,

# EASY PURESCRIPT
easy-purescript-nix ? import sources.easy-purescript-nix { },

spago2nix ? easy-purescript-nix.spago2nix,

purs ? easy-purescript-nix.purs,

# YARN2NIX

yarn2nix ? import sources.yarn2nix { },

# NIV

niv ? import sources.niv { },

# NIX LINTER

nix-linter ? import sources.nix-linter { }

}:

let
  yarnPackage = yarn2nix.mkYarnPackage {
    src = pkgs.runCommand "src" { } ''
      mkdir $out
      ln -s ${./package.json} $out/package.json
      ln -s ${./yarn.lock} $out/yarn.lock
    '';
    publishBinsFor = [ "purescript-psa" "parcel" ];
  };
in pkgs.stdenv.mkDerivation {
  name = "spago.dhall2nix";
  buildInputs = [
    yarnPackage
    dhall-json
    spago2nix
    niv.niv
    yarn
    purs
    nix
    nix-prefetch-git
    nix-linter.nix-linter
  ];
  buildCommand = ''
    mkdir $out
  '';
}
