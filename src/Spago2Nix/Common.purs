module Spago2Nix.Common where

import Prelude
import Data.Argonaut (class DecodeJson, Json, fromObject, toObject)
import Data.Argonaut as Argonaut
import Data.Array (cons, mapWithIndex)
import Data.Bifunctor (lmap)
import Data.Either (Either, note)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Map (Map)
import Data.Map as Map
import Data.Monoid (guard)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect.Exception (Error)
import Foreign.Object as Object

type NixPrefetchGitResult
  = { url :: String, rev :: String, sha256 :: String }

-- ERROR STACK
--
type ErrorStack
  = Array String

printErrorStack :: ErrorStack -> String
printErrorStack errorStack =
  ([ "Something went wrong along the way..." ] <> (errorStack <#> indent))
    # String.joinWith "\n\n"
  where
  indent str =
    str
      # String.split (Pattern "\n")
      # mapWithIndex (\i line -> (if i == 0 then " - " else "   ") <> line)
      # String.joinWith "\n"

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
jsonParser :: String -> Either ErrorStack Json
jsonParser = Argonaut.jsonParser >>> lmap (pure >>> cons "Invalid JSON")

decodeJson :: forall a. DecodeJson a => Json -> Either ErrorStack a
decodeJson = Argonaut.decodeJson >>> lmap (pure >>> cons "Invalid JSON structure")

decodeMapFromObject ::
  forall a.
  (Json -> Either ErrorStack a) ->
  Json -> Either ErrorStack (Map String a)
decodeMapFromObject decodeA json =
  toObject json
    # note [ "Expexted an object." ]
    >>= traverse decodeA
    <#> (Object.toUnfoldable :: _ -> Array _)
    >>> Map.fromFoldable

encodeMapToObject :: forall a. (a -> Json) -> Map String a -> Json
encodeMapToObject encodeA m =
  m
    <#> encodeA
    # (Map.toUnfoldable :: _ -> Array _)
    # Object.fromFoldable
    # fromObject

joinSpaces :: Array String -> String
joinSpaces = String.joinWith " "

joinStrings :: Array String -> String
joinStrings = String.joinWith ""

joinNl :: Array String -> String
joinNl = String.joinWith "\n"

foreign import implStringifyPretty :: Fn2 Int Json String

stringifyPretty :: Int -> Json -> String
stringifyPretty = runFn2 implStringifyPretty
