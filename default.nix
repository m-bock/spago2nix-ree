{

sources ? import ./nix/sources.nix,

pkgs ? import sources.nixpkgs { } }:

let

  dhall-json' = pkgs.haskellPackages.dhall-json_1_4_0.override {
    dhall = pkgs.haskellPackages.dhall_1_25_0;
  };

in let
  api = import ./nix/api.nix {
    inherit sources;
    dhall-json = dhall-json';
  };
in {
  spago2nix-ree-cli = import ./nix/pkgs/spago2nix-ree-cli.nix {
    inherit sources;
    dhall-json = dhall-json';
  };
  inherit (api) buildProject;
  inherit (api) buildProjectDependencies;
}
