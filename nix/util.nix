{

sources ? import ./sources.nix,

pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.dhall-json

}:
with builtins;
with pkgs.lib;

rec {
  defaultSpagoDhall = "spago.dhall";

  getSpagoConfig = configSrc: spagoDhall:
    pipe configSrc [
      createFiles
      (src:
        pkgs.runCommand "spago.json" { } ''
          cd ${src}/`dirname ${spagoDhall}`
          ${dhall-json}/bin/dhall-to-json --file `basename ${spagoDhall}` > $out
        '')
      readFile
      fromJSON
    ];

  getPackagesConfig = packagesLock: pipe packagesLock [ readFile fromJSON ];

  createFiles = files:
    pipe files [
      (mapAttrsToList (path: file: ''
        mkdir -p `dirname ${path}`
        pushd $_ > /dev/null
        ln -s ${file} `basename ${path}`
        popd > /dev/null
      ''))
      (concatStringsSep "\n")
      (x: ''
        mkdir $out
        cd $out
        ${x}
      '')
      (pkgs.runCommand "src" { })
    ];

  createProjectSources = { spagoConfig, src }:
    pkgs.runCommand "${spagoConfig.name}-source" { } ''
      mkdir $out

      shopt -s globstar

      cd ${src}
      for path in ${toString (spagoConfig.sources)}
      do
        mkdir -p $out/`dirname $path`
        cp $path $out/$path 
      done
    '';

  compileSpagoProject = { alreadyBuilt, projectSources }:
    pkgs.runCommand "spago-project" { } ''
      PATH=${pkgs.purs}/bin:$PATH

      tmp=`mktemp -d`
      cd $tmp

      shopt -s globstar

      cp -r --preserve=all ${alreadyBuilt}/output output
      chmod -R +w output
      cp -r --preserve=all ${alreadyBuilt}/.spago .spago

      mkdir sources
      cp -r ${projectSources}/* -t sources

      ${pkgs.psa}/bin/psa \
        --is-lib=.spago \
        --stash \
        --censor-lib \
        --strict \
        .spago/*/*/src/**/*.purs \
        sources/**/*.purs

      mkdir $out
      cp -r $tmp/output -t $out
      cp -r $tmp/.spago -t $out
    '';
}
