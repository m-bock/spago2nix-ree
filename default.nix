{ pkgs ? import (import ./nix/sources.nix).nixpkgs { }

, dhall-json ? pkgs.dhall-json

}:
pkgs.stdenv.mkDerivation {
  name = "spago.dhall2nix";
  buildInputs = [ dhall-json ];
}
