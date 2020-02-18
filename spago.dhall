{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "spago2nix-re"
, dependencies =
    [ "argonaut"
    , "codec-argonaut"
    , "console"
    , "effect"
    , "node-fs-aff"
    , "optparse"
    , "psci-support"
    , "sunde"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
