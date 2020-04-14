{ sources ? import ../sources.nix,

# PKGS

pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.dhall-json,

}:
with builtins;
let
  buildPackage = import ./build-package.nix { inherit sources; };

  getSpagoConfig = src: spagoDhall:
    fromJSON (readFile (pkgs.runCommand "spago.json" { } ''
      cd ${src}/`dirname ${spagoDhall}`
      cat `basename ${spagoDhall}` | ${dhall-json}/bin/dhall-to-json > $out
    ''));

  getPackagesConfig = packagesLock: fromJSON (readFile packagesLock);

  buildProject = {

    src,

    packagesLock,

    spagoDhall ? "spago.dhall"

    }:

    let spagoConfig = getSpagoConfig src spagoDhall;

    in buildPackage {

      package = rec {
        name = spagoConfig.name;
        dependencies = spagoConfig.dependencies;
        version = "no-version";
        source = pkgs.runCommand "${name}-source" { } ''
          mkdir $out

          shopt -s globstar

          pushd ${src}
            for path in ${toString (spagoConfig.sources)}
            do
              mkdir -p $out/src/`dirname $path`
              cp $path $out/src/$path 
            done
          popd

        '';
      };

      packagesLock = getPackagesConfig packagesLock;

    };

  buildProjectDependencies = {

    src,

    packagesLock,

    spagoDhall ? "spago.dhall"

    }:

    let spagoConfig = getSpagoConfig src spagoDhall;

    in buildPackage {

      package = rec {
        name = spagoConfig.name + "-dependencies";
        dependencies = spagoConfig.dependencies;
        version = "no-version";
        source = pkgs.runCommand "${name}-source" { } "mkdir $out";
      };

      packagesLock = getPackagesConfig packagesLock;

    };

in {
  inherit buildProject;
  inherit buildProjectDependencies;
}
