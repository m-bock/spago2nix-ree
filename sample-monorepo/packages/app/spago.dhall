{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "app"
, dependencies = [ "console", "effect", "lib1", "psci-support" ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
