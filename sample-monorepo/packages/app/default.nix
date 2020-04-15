{

pkgs ? import <nixpkgs> { },

yarn2nix ?
  import (builtins.fetchGit { url = "https://github.com/moretea/yarn2nix"; })
  { }

}:

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

in rec {
  yarnModules = yarn2nix.mkYarnModules rec {
    name = "app";
    pname = name;
    version = "1.0.0";
    packageJSON = ./package.json;
    yarnLock = ../../yarn.lock;
    yarnNix = ../../yarn.nix;
  };

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

    name = "webApp";

    inherit srcDirs;

    inherit configFiles;

    inherit spagoDhall;

    inherit packagesLock;

    node_modules = yarnModules + "/node_modules";
  };
}
