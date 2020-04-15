{

sources ? import ./sources.nix,

pkgs ? import sources.nixpkgs { },

dhall-json ? pkgs.dhall-json

}:
with builtins;
with pkgs.lib;

rec {
  defaultSpagoDhall = "spago.dhall";

  defaultEntryJS = { entryModule ? defaultEntry, outputPath ? "output" }:
    pkgs.writeText "index.js" ''
      require("./${outputPath}/${entryModule}").main();
    '';

  defaultEntryHTML = { title ? "", script, containerId }:
    pkgs.writeText "index.html" ''
      <!DOCTYPE html>
      <html>
        <head>
          <meta charset="utf-8" />
          <meta http-equiv="X-UA-Compatible" content="IE=edge" />
          <title>${title}</title>
          <meta name="viewport" content="width=device-width, initial-scale=1" />
        </head>
        <body>
          <div id="${containerId}"></div>
          <script src="${script}"></script>
        </body>
      </html>
    '';

  defaultEntry = "Main";

  emptyDir = pkgs.runCommand "empty-dir" { } "mkdir $out";

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

  compileSpagoProject = { projectDepenedencies, projectSources }:
    pkgs.runCommand "spago-project" { } ''
      PATH=${pkgs.purs}/bin:$PATH

      tmp=`mktemp -d`
      cd $tmp

      shopt -s globstar

      cp -r --preserve=all ${projectDepenedencies}/output output

      chmod -R +w output
      cp -r --preserve=all ${projectDepenedencies}/.spago .spago

      mkdir sources
      cp -rL ${projectSources}/* -t sources

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

  buildParcelNode = { src, entry, node_modules }:
    pkgs.runCommand "parcel-bundle" { } ''
      tmp=`mktemp -d`
      cd $tmp
      ln -s ${src}/* -t .
      ${pkgs.parcel}/bin/parcel build --target node --no-source-maps ${entry}

      mkdir $out
      cp -r $tmp/dist/* -t $out
      ln -s ${node_modules}  $out/node_modules
      ln -s ${src}/output $out/output
    '';

  createNodeBinary = { name, src }:
    pkgs.stdenv.mkDerivation {
      inherit name;

      buildCommand = ''
        mkdir -p $out/bin
        cp -r ${src}/node_modules -t $out/bin
        cp -r ${src}/output -t $out/bin

        echo "#!${pkgs.nodejs}/bin/node" > $out/bin/${name}

        cat ${src}/index.js >> $out/bin/${name}

        chmod +x $out/bin/${name}
      '';
    };

  buildParcelWeb = { src, entry, node_modules }:
    pkgs.runCommand "parcel-bundle" { } ''
      tmp=`mktemp -d`
      cd $tmp
      ln -s ${src}/* -t .
      ln -s ${node_modules} ./node_modules
      ${pkgs.parcel}/bin/parcel build --no-source-maps ${entry}

      mkdir $out
      cp -r $tmp/dist/* -t $out
    '';
}
