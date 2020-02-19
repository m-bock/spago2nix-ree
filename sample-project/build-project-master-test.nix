{ pkgs ? import <nixpkgs> { } }:
let
  spago2nix-ree = import
    (builtins.fetchGit { url = "https://github.com/thought2/spago2nix-ree"; })
    { };
in pkgs.stdenv.mkDerivation {

  name = "my-project";

  buildCommand = ''
    ln -s ${spago2nix-ree.buildProject { src = ./.; }} $out
  '';

  buildInputs = [ spago2nix-ree.spago2nix-ree ];
}

