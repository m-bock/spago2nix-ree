{

sources ? import ./nix/sources.nix,

pkgs ? import sources.nixpkgs { } }:

let

  pkgs' = pkgs // {
    lib = pkgs.lib // {
      pipe = val: functions:
        let reverseApply = x: f: f x;
        in builtins.foldl' reverseApply val functions;
    };
    dhall-json = pkgs.haskellPackages.dhall-json_1_4_0.override {
      dhall = pkgs.haskellPackages.dhall_1_25_0;
    };
  };

in let api = import ./nix/api.nix { pkgs = pkgs'; };
in {
  spago2nix-ree-cli = import ./nix/pkgs/spago2nix-ree-cli.nix {
    inherit sources;
    dhall-json = pkgs'.dhall-json;
  };
  inherit (api) buildProject;
  inherit (api) buildProjectDependencies;
}
