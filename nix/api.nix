{ sources ? import ./sources.nix,

# PKGS

pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.dhall-json

}:
with builtins;
with pkgs.lib;
let
  buildPackage = import ./build-package.nix { inherit sources; };
  util = import ./util.nix { inherit pkgs; };

in rec {

  buildProjectDependencies = {

    configSrc,

    packagesLock,

    spagoDhall ? util.defaultSpagoDhall

    }:

    let
      spagoConfig = util.getSpagoConfig configSrc spagoDhall;

      buildPackageConfig = {

        package = rec {
          name = spagoConfig.name + "-dependencies";
          dependencies = spagoConfig.dependencies;
          version = "no-version";
          source = pkgs.runCommand "${name}-source" { } "mkdir $out";
        };

        packagesLock = util.getPackagesConfig packagesLock;

      };

    in buildPackage buildPackageConfig;

  buildProject = {

    src,

    configSrc,

    packagesLock,

    spagoDhall ? util.defaultSpagoDhall

    }:

    let
      spagoConfig = util.getSpagoConfig configSrc spagoDhall;

      src' = util.createFiles src;

      projectDepenedencies = buildProjectDependencies {
        inherit configSrc;
        inherit spagoDhall;
        inherit packagesLock;
      };

      projectSources = util.createProjectSources {
        inherit spagoConfig;
        src = src';
      };

      compileSpagoProjectConfig = {
        alreadyBuilt = projectDepenedencies;
        projectSources = projectSources;
      };

    in util.compileSpagoProject compileSpagoProjectConfig;
}
