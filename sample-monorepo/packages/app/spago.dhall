{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "app"
, dependencies = [ "console", "effect", "psci-support", "lib1" ]
, packages = ../../packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
