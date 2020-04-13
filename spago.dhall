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
  , "parsing"
  , "pathy"
  , "simple-text"
  , "sunde"
  , "uri"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
