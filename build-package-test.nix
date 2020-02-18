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
  buildPackage = import ./build-package.nix { };

  spagoPackages = {
    foo = {
      name = "foo";
      version = "v0.0.0";
      source = pkgs.runCommand "src" { } ''
        mkdir -p $out/src
        echo 'module Foo where' > $out/src/Foo.purs
      '';
      dependencies = [ ];
    };
    bar = {
      name = "bar";
      version = "v0.0.0";
      source = pkgs.runCommand "src" { } ''
        mkdir -p $out/src
        echo 'module Bar where import Foo' > $out/src/Bar.purs
      '';
      dependencies = [ "foo" ];
    };
    baz = {
      name = "baz";
      version = "v0.0.0";
      source = pkgs.runCommand "src" { } ''
        mkdir -p $out/src
        echo 'module Baz where import Foo' > $out/src/Baz.purs
      '';
      dependencies = [ "foo" ];
    };
  };

in {
  bar = buildPackage {
    inherit spagoPackages;
    package = spagoPackages.bar;
  };
  baz = buildPackage {
    inherit spagoPackages;
    package = spagoPackages.baz;
  };
}
