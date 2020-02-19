
{ name =
    "spago2nix-ree"
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
