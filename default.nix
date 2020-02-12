let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  easy-purescript-nix' = import sources.easy-purescript-nix { pkgs = nixpkgs; };

  niv' = import sources.niv { pkgs = nixpkgs; };
in {
# PKGS
pkgs ? nixpkgs,

dhall-json ? pkgs.dhall-json,

# EASY PURESCRIPT
easy-purescript-nix ? easy-purescript-nix',

spago2nix ? easy-purescript-nix'.spago2nix,

# NIV
niv ? niv'

}:
pkgs.stdenv.mkDerivation {
  name = "spago.dhall2nix";
  buildInputs = [ dhall-json spago2nix niv.niv ];
  buildCommand = ''
    mkdir $out
  '';
}
