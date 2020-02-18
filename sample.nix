let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

in { pkgs ? nixpkgs }:

let spago2nix = import ./default.nix { };

in spago2nix.buildProject { }
