let
  sources = import ./nix/sources.nix;

  nixpkgs = import sources.nixpkgs { };

  easy-purescript-nix' = import sources.easy-purescript-nix { pkgs = nixpkgs; };

  yarn2nix' = import sources.yarn2nix { pkgs = nixpkgs; };

in {
# PKGS
pkgs ? nixpkgs,

dhall-json ? pkgs.dhall-json,

nodejs ? pkgs.nodejs,

yarn ? pkgs.yarn,

nixfmt ? nixpkgs.nixfmt,

nix-prefetch-git ? nixpkgs.nix-prefetch-git,

jq ? pkgs.jq,

# EASY PURESCRIPT
easy-purescript-nix ? easy-purescript-nix',

spago2nix ? easy-purescript-nix.spago2nix,

purs ? easy-purescript-nix.purs,

# YARN2NIX

yarn2nix ? yarn2nix'

}:

let
  yarnPackage = yarn2nix.mkYarnPackage {
    src = pkgs.runCommand "src" { } ''
      mkdir $out

      ln -s ${./package.json} $out/package.json
      ln -s ${./yarn.lock} $out/yarn.lock
    '';

    publishBinsFor = [ "purescript-psa" "parcel" ];
  };

  buildProject = { spagoPackages, spagoConfig }:
    let
      projectPackage = {
        name = "foo";
        dependencies = [ ];
        source = pkgs.runCommand "source" "";
      };
    in buildPackage {
      inherit spagoPackages;
      package = projectPackage;
    };

  buildPackage = { spagoPackages, package }:
    let

      dependencies =
        map (key: builtins.getAttr key spagoPackages) package.dependencies;

      source = pkgs.fetchgit package.git;

      dependenciesBuilt = map (dep:
        buildPackage {
          inherit spagoPackages;
          entry = dep.name;
        }) dependencies;

      forEach = f: xs: builtins.concatStringsSep "\n" (map f xs);

    in rec {
      sources = pkgs.runCommand "purescript-${package.name}-sources" { } ''
        mkdir $out

        # Link in dependency sources
        ${forEach (dep: "ln -sf ${dep.sources}/* -t $out") dependenciesBuilt}

        # Link in own sources
        dir=$out/${package.name}
        mkdir $dir
        ln -s ${source} $dir/${package.version} 
      '';
      output = pkgs.runCommand "purescript-${package.name}-output" { } ''
        tmp=`mktemp -d`

        # Link in sources
        ln -s ${sources} $tmp/.spago

        mkdir $tmp/output

        # Link in dependency builds
        ${forEach (dep: "cp -ru ${dep.output}/* -t $tmp/output")
        dependenciesBuilt}
        rm -f $tmp/output/cache-db.json

        chmod -R +w $tmp/output

        # Shallow merge of cache-db json files
        ${jq}/bin/jq -s \
            'reduce .[] as $item ({}; . + $item)' \
            ${
              toString
              (map (dep: "${dep.output}/cache-db.json") dependenciesBuilt)
            } \
            > $tmp/output/cache-db.json

        # Compile
        cd $tmp
        ${purs}/bin/purs compile '.spago/*/*/src/**/*.purs'

        mkdir $out
        cp -r $tmp/output/* -t $out
      '';
    };
in {
  spago2nix = import ./spago2nix-cli.nix { };
  inherit buildProject;
  inherit buildPackage;
}
