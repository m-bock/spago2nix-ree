module Spago2Nix.Common where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser)
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Monoid (guard)
import Data.String as String
import Effect.Exception (Error)

-- ERROR STACK
--
type ErrorStack
  = Array String

printErrorStack :: ErrorStack -> String
printErrorStack errorStack =
  ([ "Something went wrong along the way..." ] <> (errorStack <#> (" - " <> _)))
    # String.joinWith "\n\n"

nativeErrorToStack :: Boolean -> Error -> ErrorStack
nativeErrorToStack debug unknownError =
  [ "Unknown error." ]
    <> guard debug [ show unknownError ]

-- STRING FORMATTING
--
tick :: String -> String
tick str = "`" <> str <> "`"

-- UTIL 
--
decodeJsonFromString :: forall a. DecodeJson a => String -> Either ErrorStack a
decodeJsonFromString =
  (>=>)
    (jsonParser >>> lmap (pure >>> cons "Invalid JSON"))
    (decodeJson >>> lmap (pure >>> cons "Invalid Structure"))
