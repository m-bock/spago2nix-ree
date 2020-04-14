let sources = import ./nix/sources.nix;

in {

# PKGS

pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.dhall-json,

nodejs ? pkgs.nodejs,

yarn ? pkgs.yarn,

nixfmt ? pkgs.nixfmt,

nix-prefetch-git ? pkgs.nix-prefetch-git,

jq ? pkgs.jq,

# EASY PURESCRIPT

easy-purescript-nix ? import sources.easy-purescript-nix { inherit pkgs; },

spago2nix ? easy-purescript-nix.spago2nix,

purs ? easy-purescript-nix.purs,

# YARN2NIX

yarn2nix ? import sources.yarn2nix { inherit pkgs; },

}:
with builtins;
let

  resolvePackage = package:
    let
      source = if package.tag == "remote" then
        let
          repo = pkgs.fetchgit {
            sha256 = package.value.nixSha256;
            url = package.value.repo;
            inherit (package.value) rev;
          };
        in pkgs.runCommand "src" { } "ln -s ${repo}/src $out"
      else if package.tag == "local" then
        package.value + "/src"
      else
        { };

    in {
      name = package.value.name;
      inherit source;
    };

  buildPackage = import ./build-package.nix { };

  buildProject = {

    src,

    packagesLock,

    spagoDhall ? "spago.dhall"

    }:
    let

      spagoConfig = fromJSON (readFile (pkgs.runCommand "spago.json" { } ''
        cd ${src}/`dirname ${spagoDhall}`
        cat `basename ${spagoDhall}` | ${dhall-json}/bin/dhall-to-json > $out
      ''));

      packagesLock' = fromJSON (readFile packagesLock);

      projectPackage = rec {
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

      spagoPackages = mapAttrs (_: resolvePackage) packagesLock';

    in buildPackage {
      inherit spagoPackages;
      package = projectPackage;
    };

  buildProjectDependencies = {

    src,

    packagesLock,

    spagoDhall ? "spago.dhall"

    }:
    let

      spagoConfig = fromJSON (readFile (pkgs.runCommand "spago.json" { } ''
        cd ${src}/`dirname ${spagoDhall}`
        cat `basename ${spagoDhall}` | ${dhall-json}/bin/dhall-to-json > $out
      ''));

      projectPackage = rec {
        name = spagoConfig.name + "-dependencies";
        dependencies = spagoConfig.dependencies;
        version = "no-version";
        source = pkgs.runCommand "${name}-source" { } "mkdir $out";
      };

      spagoPackages = mapAttrs (_: resolvePackage) packagesLock;

    in buildPackage {
      inherit spagoPackages;
      package = projectPackage;
    };

in {
  spago2nix-ree = import ./spago2nix-ree-cli.nix { };
  inherit buildProject;
  inherit buildProjectDependencies;
}
