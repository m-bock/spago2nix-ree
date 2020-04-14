{ pkgs ? import <nixpkgs> { } }:

let spago2nix-ree = import ../../../default.nix { };
in spago2nix-ree.buildProjectDependencies {
  src = pkgs.runCommand "src" { } ''
    mkdir $out
    cd $out

    ln -s ${../../packages-upstream.dhall} ./packages-upstream.dhall
    ln -s ${../../packages.dhall} ./packages.dhall

    mkdir -p packages/app
    pushd $_ > /dev/null
    ln -s ${./spago.dhall} ./spago.dhall
    popd > /dev/null
  '';

  spagoDhall = "packages/app/spago.dhall";

  packagesLock = ../../packages-lock.json;
}
