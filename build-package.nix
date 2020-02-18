let sources = import ./nix/sources.nix;

in {
# PKGS
pkgs ? import sources.nixpkgs { },

jq ? pkgs.jq,

# EASY PURESCRIPT
easy-purescript-nix ? import sources.easy-purescript-nix { },

purs ? easy-purescript-nix.purs,

}:
with builtins;
let
  forEach = f: xs: builtins.concatStringsSep "\n" (map f xs);

  buildPackage = { spagoPackages, package }:
    let

      dependencies =
        map (key: builtins.getAttr key spagoPackages) package.dependencies;

      source = package.source;

      dependenciesBuilt = map (dep:
        buildPackage {
          inherit spagoPackages;
          package = dep;
        }) dependencies;

    in pkgs.runCommand "purescript-${package.name}" { } ''
      mkdir $out

      mkdir $out/.spago
      mkdir $out/output

      # Link in dependency sources
      ${forEach (dep: "cp -ru ${dep}/.spago/* -t $out/.spago")
      dependenciesBuilt}

      # Link in own sources
      dir=$out/.spago/${package.name}
      mkdir $dir
      cp -r ${source} $dir/${package.version} 

      # Link in dependency builds
      ${forEach (dep: "cp -ru ${dep}/output/* -t $out/output")
      dependenciesBuilt}
      rm -f $out/output/cache-db.json

      chmod -R +w $out/output

      # Shallow merge of cache-db json files
      ${jq}/bin/jq -s \
          'reduce .[] as $item ({}; . + $item)' \
          ${
            toString
            (map (dep: "${dep}/output/cache-db.json") dependenciesBuilt)
          } \
          > $out/output/cache-db.json

      chmod -R +w $out/output

      cd $out
      ${purs}/bin/purs compile '.spago/*/*/src/**/*.purs'
    '';

in buildPackage
