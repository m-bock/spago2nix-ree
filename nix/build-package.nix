{ sources ? import ./sources.nix,

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

  buildPackage = { packagesLock, package }:
    (buildPackage' {
      spagoPackages = mapAttrs (_: resolvePackage) packagesLock;
      inherit package;
      cache = { };
    }).package;

  buildPackage' = { spagoPackages, package, cache }:
    let

      dependencies =
        map (key: builtins.getAttr key spagoPackages) package.dependencies;

      source = package.source;

      dependenciesBuilt = builtins.foldl' (memo: unbuildPackage:

        if (builtins.hasAttr unbuildPackage.name memo.cache) then

        {
          cache = memo.cache;
          packages = [ (builtins.getAttr unbuildPackage.name memo.cache) ]
            ++ memo.packages;
        }

        else
          let
            result = buildPackage' {
              inherit spagoPackages;
              cache = memo.cache;
              package = unbuildPackage;
            };
          in {
            cache = result.cache
              // (pkgs.lib.setAttrByPath [ unbuildPackage.name ]
                result.package);
            packages = [ result.package ] ++ memo.packages;
          }

      ) {
        inherit cache;
        packages = [ ];
      } dependencies;

    in {
      package = pkgs.runCommand "purescript-${package.name}" { } ''
        mkdir $out

        mkdir $out/.spago
        mkdir $out/output

        # Link in dependency sources
        ${forEach (dep: "cp -ru ${dep}/.spago/* -t $out/.spago")
        dependenciesBuilt.packages}

        # Link in own sources
        dir=$out/.spago/${package.name}/${package.version}
        mkdir -p $dir
        cp -r ${source} $dir/src 

        # Link in dependency builds
        ${forEach (dep: "cp -ru --preserve=all ${dep}/output/* -t $out/output")
        dependenciesBuilt.packages}
        rm -f $out/output/cache-db.json

        chmod -R +w $out/output

        # Shallow merge of cache-db json files
        ${jq}/bin/jq -s \
            'reduce .[] as $item ({}; . + $item)' \
            ${
              toString (map (dep: "${dep}/output/cache-db.json")
                (pkgs.lib.reverseList dependenciesBuilt.packages))
            } \
            > $out/output/cache-db.json

        chmod -R +w $out/output

        cd $out
        ${purs}/bin/purs compile '.spago/*/*/src/**/*.purs'
      '';
      cache = dependenciesBuilt.cache;
    };

in buildPackage
