{ pkgs ? import <nixpkgs> { } }:
let
  spago2nix-ree = import (builtins.fetchGit {

    url = "https://github.com/thought2/spago2nix-ree";

    # ... ideally you'd specify a concrete revision here

  }) { };

  spagoProject = spago2nix-ree.buildProject {

    src = ./.;

  };

in pkgs.stdenv.mkDerivation {

  name = "my-project";

  buildCommand = ''
    ln -s ${spagoProject} $out
  '';

  buildInputs = [
    # Puts the CLI on the path in a nix shell
    spago2nix-ree.spago2nix-ree
  ];
}

