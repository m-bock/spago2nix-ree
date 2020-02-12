let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  easy-purescript-nix' = import sources.easy-purescript-nix { pkgs = nixpkgs; };

  niv' = import sources.niv { pkgs = nixpkgs; };

  yarn2nix' = import sources.yarn2nix { pkgs = nixpkgs; };
in {
# PKGS
pkgs ? nixpkgs,

dhall-json ? pkgs.dhall-json,

yarn ? pkgs.yarn,

nix ? pkgs.nix,

# EASY PURESCRIPT
easy-purescript-nix ? easy-purescript-nix',

spago2nix ? easy-purescript-nix'.spago2nix,

purs ? easy-purescript-nix'.purs,

# NIV
niv ? niv',

# YARN2NIX

yarn2nix ? yarn2nix'

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
  buildInputs = [ yarnPackage dhall-json spago2nix niv.niv yarn purs nix ];
  buildCommand = ''
    mkdir $out
  '';
}
