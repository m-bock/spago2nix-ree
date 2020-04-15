{ pkgs ? import <nixpkgs> { } }:

let spago2nix-ree = import ../../../default.nix { };
in {
  projectDependencies = spago2nix-ree.buildProjectDependencies {

    configFiles = {
      "packages.dhall" = ../../packages.dhall;
      "packages-upstream.dhall" = ../../packages-upstream.dhall;
      "packages/app/spago.dhall" = ./spago.dhall;
    };

    spagoDhall = "packages/app/spago.dhall";

    packagesLock = ../../packages-lock.json;
  };

  project = spago2nix-ree.buildProject {

    srcDirs = {
      "src" = ./src;
      "test" = ./test;
    };

    configFiles = {
      "packages.dhall" = ../../packages.dhall;
      "packages-upstream.dhall" = ../../packages-upstream.dhall;
      "packages/app/spago.dhall" = ./spago.dhall;
    };

    spagoDhall = "packages/app/spago.dhall";

    packagesLock = ../../packages-lock.json;
  };

  cli = spago2nix-ree.buildCLI {

    srcDirs = {
      "src" = ./src;
      "test" = ./test;
    };

    configFiles = {
      "packages.dhall" = ../../packages.dhall;
      "packages-upstream.dhall" = ../../packages-upstream.dhall;
      "packages/app/spago.dhall" = ./spago.dhall;
    };

    spagoDhall = "packages/app/spago.dhall";

    packagesLock = ../../packages-lock.json;
  };

  webApp = spago2nix-ree.buildWebApp {

    srcDirs = {
      "src" = ./src;
      "test" = ./test;
    };

    configFiles = {
      "packages.dhall" = ../../packages.dhall;
      "packages-upstream.dhall" = ../../packages-upstream.dhall;
      "packages/app/spago.dhall" = ./spago.dhall;
    };

    spagoDhall = "packages/app/spago.dhall";

    packagesLock = ../../packages-lock.json;
  };
}
