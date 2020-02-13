module Spago2Nix.SpagoConfig (SpagoConfig, toNix) where

import Prelude
import Data.Argonaut (class DecodeJson, Json, decodeJson, toObject, (.:))
import Data.Either (Either, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Foreign.Object (keys, lookup)
import NixAST (NixAST)
import Partial.Unsafe (unsafeCrashWith)
import Spago2Nix.Common (tick)

-- SPAGO CONFIG
--
newtype SpagoConfig
  = SpagoConfig
  { name :: String
  , dependencies :: Array String
  , packages ::
    Map String
      { dependencies :: Array String
      , version :: String
      , repo :: String
      }
  , sources :: Array String
  }

instance decodeJsonSpagoConfig :: DecodeJson SpagoConfig where
  decodeJson json = do
    _obj <- toObject json # note "Expected an object."
    name <- _obj .: "name"
    dependencies <- _obj .: "dependencies"
    packages <- "packages" `lookup` _obj # note "Expected key `packages`." >>= (decodeMap decodeJson)
    sources <- _obj .: "sources"
    pure $ SpagoConfig { name, dependencies, packages, sources }

decodeMap :: forall a. (Json -> Either String a) -> Json -> Either String (Map String a)
decodeMap decodeA json = do
  obj <- toObject json # note "Expexted an object."
  pairs <-
    for (keys obj) \key ->
      lookup key obj # note ("Expected key " <> tick key <> ".") >>= decodeA <#> (\v -> Tuple key v)
  pure $ Map.fromFoldable pairs

toNix :: SpagoConfig -> NixAST
toNix = \_ -> unsafeCrashWith "TODO: generateNix"
