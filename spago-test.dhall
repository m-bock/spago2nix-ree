let
  config = ./spago.dhall
in
{ name = "spago2nix-ree-test"
, dependencies =
  [ "psci-support"
  ] # config.dependencies
, packages = ./packages.dhall
, sources = [ "test/**/*.purs" ] # config.sources
}
