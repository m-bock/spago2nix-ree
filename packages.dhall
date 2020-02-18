let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.6-20200127/packages.dhall sha256:06a623f48c49ea1c7675fdf47f81ddb02ae274558e29f511efae1df99ea92fb8

let overrides = { metadata = upstream.metadata ⫽ { version = "v0.13.4" } }

let additions =
      { codec-argonaut =
          { repo =
              "https://github.com/garyb/purescript-codec-argonaut.git"
          , version =
              "v7.1.0"
          , dependencies =
              [ "ordered-collections"
              , "variant"
              , "type-equality"
              , "argonaut-core"
              , "codec"
              , "generics-rep"
              ]
          }
      , codec =
          { repo =
              "https://github.com/garyb/purescript-codec.git"
          , version =
              "v3.0.0"
          , dependencies =
              [ "profunctor", "transformers" ]
          }
      }

in  upstream ⫽ overrides ⫽ additions
