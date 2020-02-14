module NixAST (NixExpr(..), print) where

import Prelude
import Data.String as String
import Data.Tuple.Nested (type (/\), (/\))
import Spago2Nix.Common (joinSpaces, joinStrings)

data NixExpr
  = AttrSet (Array (String /\ NixExpr))
  | App NixExpr (Array NixExpr)
  | List (Array NixExpr)
  | DotAccess (Array String)
  | String String

print :: NixExpr -> String
print (AttrSet fields) =
  joinStrings
    [ "{"
    , fields
        >>= ( \(key /\ value) ->
              [ quotes key, "=", print value, ";" ]
          )
        # joinStrings
    , "}"
    ]

print (App expr exprs) =
  joinSpaces
    $ [ print expr ]
    <> (exprs <#> print)

print (String value) = quotes value

print (DotAccess steps) = String.joinWith "." steps

print (List entries) =
  joinStrings
    [ "["
    , entries <#> print # joinSpaces
    , "]"
    ]

quotes :: String -> String
quotes value = joinStrings [ "\"", value, "\"" ]
