{ pkgs ? import <nixpkgs> { } }:

let spago2nix-ree = import ../../../default.nix { };
in {
  projectDependencies = spago2nix-ree.buildProjectDependencies {

    configSrc = {
      "packages.dhall" = ../../packages.dhall;
      "packages-upstream.dhall" = ../../packages-upstream.dhall;
      "packages/app/spago.dhall" = ./spago.dhall;
    };

    spagoDhall = "packages/app/spago.dhall";

    packagesLock = ../../packages-lock.json;
  };

  project = spago2nix-ree.buildProject {

    src = {
      "src" = ./src;
      "test" = ./test;
    };

    configSrc = {
      "packages.dhall" = ../../packages.dhall;
      "packages-upstream.dhall" = ../../packages-upstream.dhall;
      "packages/app/spago.dhall" = ./spago.dhall;
    };

    spagoDhall = "packages/app/spago.dhall";

    packagesLock = ../../packages-lock.json;
  };
}
