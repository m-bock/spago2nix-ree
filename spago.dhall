{ name = "spago2nix-ree"
, dependencies =
  [ "argonaut"
  , "codec-argonaut"
  , "console"
  , "effect"
  , "env-vars"
  , "foreign-object"
  , "node-fs-aff"
  , "optparse"
  , "pathy"
  , "simple-text"
  , "sunde"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
