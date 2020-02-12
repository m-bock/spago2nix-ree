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

# EASY PURESCRIPT
easy-purescript-nix ? easy-purescript-nix',

spago2nix ? easy-purescript-nix'.spago2nix,

purs ? easy-purescript-nix'.purs,

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

  spago2nix = pkgs.stdenv.mkDerivation {
    name = "spago2nix";

    phases = [
      "preBuildPhase"
      "buildPhase"
      "checkPhase"
      "installPhase"
      "preFixupPhase"
      "fixupPhase"
      "installCheckPhase"
    ];

    buildInputs = [ yarnPackage purs nodejs yarn pkgs.makeWrapper ];

    doCheck = true;

    doInstallCheck = true;

    src = pkgs.runCommand "src" { } ''
      mkdir $out

      ln -s ${./Makefile} $out/Makefile
      ln -s ${./src} $out/src
      ln -s ${./test} $out/test
    '';

    preBuildPhase = ''
      TMP=`mktemp -d`
      cd $TMP

      ln -s $src/* -t .
      bash ${(pkgs.callPackage ./spago-packages.nix { }).installSpagoStyle}
    '';

    installPhase = ''
      mkdir $out
      cp -r $TMP/dist/* -t $out 
    '';

    preFixupPhase = ''
      mv $out/bin/spago2nix $out/bin/spago2nix-unpure

      makeWrapper $out/bin/spago2nix-unpure $out/bin/spago2nix \
        --set PURE true \
        --set DHALL_TO_JSON ${dhall-json}/bin/dhall-to-json \
        --set NIX_FORMAT ${nixfmt}/bin/nixfmt \
        --set NIX_PREFETCH_GIT ${nix-prefetch-git}/bin/nix-prefetch-git
    '';

  };
in { inherit spago2nix; }
