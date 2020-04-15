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

    configFiles,

    packagesLock,

    spagoDhall ? util.defaultSpagoDhall

    }:

    let
      spagoConfig = util.getSpagoConfig configFiles spagoDhall;

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

    srcDirs,

    configFiles,

    packagesLock,

    spagoDhall ? util.defaultSpagoDhall

    }:

    let
      spagoConfig = util.getSpagoConfig configFiles spagoDhall;

      src = util.createFiles srcDirs;

      projectDepenedencies = buildProjectDependencies {
        inherit configFiles;
        inherit spagoDhall;
        inherit packagesLock;
      };

      projectSources = util.createProjectSources {
        inherit spagoConfig;
        inherit src;
      };

      compileSpagoProjectConfig = {
        inherit projectDepenedencies;
        inherit projectSources;
      };

    in util.compileSpagoProject compileSpagoProjectConfig;

  buildCLI = {

    name ? let

      spagoConfig = util.getSpagoConfig configFiles spagoDhall;
    in spagoConfig.name,

    srcDirs,

    configFiles,

    packagesLock,

    spagoDhall ? util.defaultSpagoDhall,

    entryModule ? util.defaultEntry,

    node_modules ? util.emptyDir }:

    let

      project = buildProject {
        inherit srcDirs;
        inherit configFiles;
        inherit packagesLock;
        inherit spagoDhall;
      };

      buildParcelConfigNode = {
        src = util.createFiles {
          "." = project + "/*";
          "index.js" = util.defaultEntryJS { inherit entryModule; };
        };
        entry = "index.js";
        inherit node_modules;
      };

    in pipe buildParcelConfigNode [
      util.buildParcelNode
      (src:
        util.createNodeBinary {
          inherit src;
          inherit name;
        })
    ];

  buildWebApp = {

    title ? "",

    srcDirs,

    configFiles,

    packagesLock,

    spagoDhall ? util.defaultSpagoDhall,

    entryModule ? util.defaultEntry,

    node_modules ? util.emptyDir,

    containerId ? "app"

    }:

    let

      project = buildProject {
        inherit srcDirs;
        inherit configFiles;
        inherit packagesLock;
        inherit spagoDhall;
      };

      buildParcelConfigWeb = {
        src = util.createFiles {
          "." = project + "/*";
          "index.js" = util.defaultEntryJS { inherit entryModule; };
          "index.html" = util.defaultEntryHTML {
            inherit title;
            script = "index.js";
            inherit containerId;
          };
        };
        entry = "index.html";
        inherit node_modules;
      };

    in util.buildParcelWeb buildParcelConfigWeb;

}
