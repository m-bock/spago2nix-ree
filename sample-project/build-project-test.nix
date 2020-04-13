{ pkgs ? import <nixpkgs> { } }:

let spago2nix-ree = import ../default.nix { };
in spago2nix-ree.buildProject {
  src = pkgs.runCommand "src" { } ''
    mkdir $out
    ln -s ${./spago.dhall} $out/spago.dhall
    ln -s ${./packages.dhall} $out/packages.dhall
    ln -s ${./packages-upstream.dhall} $out/packages-upstream.dhall
  '';

  packagesLock = ./packages-lock.json;
}
