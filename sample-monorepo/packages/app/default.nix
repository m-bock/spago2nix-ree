{ pkgs ? import <nixpkgs> { } }:

let

  spago2nix-ree = import ../../../default.nix { };

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

in {
  projectDependencies = spago2nix-ree.buildProjectDependencies {

    inherit configFiles;

    inherit spagoDhall;

    inherit packagesLock;
  };

  project = spago2nix-ree.buildProject {

    inherit srcDirs;

    inherit configFiles;

    inherit spagoDhall;

    inherit packagesLock;
  };

  cli = spago2nix-ree.buildCLI {

    inherit srcDirs;

    inherit configFiles;

    inherit spagoDhall;

    inherit packagesLock;
  };

  webApp = spago2nix-ree.buildWebApp {

    inherit srcDirs;

    inherit configFiles;

    inherit spagoDhall;

    inherit packagesLock;
  };
}
