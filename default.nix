{

sources ? import ./nix/sources.nix,

pkgs ? import sources.nixpkgs { },

yarn2nix ? import sources.yarn2nix { },

easy-purescript-nix ? import sources.easy-purescript-nix { },

purs ? easy-purescript-nix.purs

}:

let

  lib' = pkgs.lib // {
    pipe = val: functions:
      let reverseApply = x: f: f x;
      in builtins.foldl' reverseApply val functions;
  };

  pkgs' = pkgs // {

    lib = lib';

    dhall-json = pkgs.haskellPackages.dhall-json_1_4_0.override {
      dhall = pkgs.haskellPackages.dhall_1_25_0;
    };

    psa = lib'.pipe {

      src = pkgs.runCommand "src" { } ''
        mkdir $out
        ln -s ${./package.json} $out/package.json
        ln -s ${./yarn.lock} $out/yarn.lock
      '';

      yarnNix = ./yarn.nix;

      publishBinsFor = [ "purescript-psa" ];

    } [
      yarn2nix.mkYarnPackage
      (yarnPackage:
        pkgs.writeShellScriptBin "psa" ''
          ${yarnPackage}/bin/psa $@
        '')
    ];

    parcel = lib'.pipe {

      src = pkgs.runCommand "src" { } ''
        mkdir $out
        ln -s ${./package.json} $out/package.json
        ln -s ${./yarn.lock} $out/yarn.lock
      '';

      yarnNix = ./yarn.nix;

      publishBinsFor = [ "parcel" ];

    } [
      yarn2nix.mkYarnPackage
      (yarnPackage:
        pkgs.writeShellScriptBin "parcel" ''
          ${yarnPackage}/bin/parcel $@
        '')
    ];

    purs = purs;

  };

in let api = import ./nix/api.nix { pkgs = pkgs'; };
in {
  spago2nix-ree-cli = import ./nix/pkgs/spago2nix-ree-cli.nix {
    inherit sources;
    dhall-json = pkgs'.dhall-json;
  };
  inherit (api) buildProject;
  inherit (api) buildProjectDependencies;
  inherit (api) buildCLI;
  inherit (api) buildWebApp;
  inherit (api) getPackages;
  inherit (api) writePureScriptBin;
}
