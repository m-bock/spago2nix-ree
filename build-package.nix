let sources = import ./nix/sources.nix;

in {
# PKGS
pkgs ? import sources.nixpkgs { },

jq ? pkgs.jq,

# EASY PURESCRIPT
easy-purescript-nix ? import sources.easy-purescript-nix { },

purs ? easy-purescript-nix.purs,

}:

let
  forEach = f: xs: builtins.concatStringsSep "\n" (map f xs);

  buildPackage = { spagoPackages, package }:
    let

      dependencies =
        map (key: builtins.getAttr key spagoPackages) package.dependencies;

      source = pkgs.fetchgit {
        repo = package.url;
        inherit (package) sha256 rev;
      };

      dependenciesBuilt = map (dep:
        buildPackage {
          inherit spagoPackages;
          package = dep;
        }) dependencies;

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

in buildPackage
