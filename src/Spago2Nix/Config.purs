module Spago2Nix.Config
  ( Config
  , EnvVars
  , CliArgs
  , parseEnvDebug
  , parseEnvVars
  , cliParserInfo
  ) where

import Prelude
import Spago2Nix.Common (ErrorStack, tick)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Foreign.Object (Object)
import Foreign.Object as Object
import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper, info, long, metavar, progDesc, showDefault, strOption, value, (<**>))

type Config
  = { | EnvVars (CliArgs ()) }

type EnvVars r
  = ( debug :: Boolean
    , pure :: Boolean
    , dhallToJson :: String
    , nixFormat :: String
    , nixPrefetchGit :: String
    | r
    )

type CliArgs r
  = ( spagoPackages :: String
    , target :: String
    | r
    )

parseCliArgs :: Parser { | CliArgs () }
parseCliArgs =
  (\spagoPackages target -> { spagoPackages, target })
    <$> strOption
        ( long "spagoPackages"
            <> metavar "SPAGO_PACKAGES"
            <> help "Path to spago packages file"
            <> showDefault
            <> value "packages.dhall"
        )
    <*> strOption
        ( long "target"
            <> metavar "TARGET"
            <> help "Path to target file"
            <> showDefault
            <> value "packages-lock.json"
        )

parseEnvVars :: Object String -> Either ErrorStack { | EnvVars () }
parseEnvVars obj = do
  _debug <-
    lookupEnv
      { name: "DEBUG"
      , default: Just false
      , parse: envBool
      }
      obj
  _pure <-
    lookupEnv
      { name: "PURE"
      , default: Just false
      , parse: envBool
      }
      obj
  _dhallToJson <-
    lookupEnv
      { name: "DHALL_TO_JSON"
      , default: guard (not _pure) Just "dhall-to-json"
      , parse: envString
      }
      obj
  _nixFormat <-
    lookupEnv
      { name: "NIX_FORMAT"
      , default: guard (not _pure) Just "nixfmt"
      , parse: envString
      }
      obj
  _nixPrefetchGit <-
    lookupEnv
      { name: "NIX_PREFETCH_GIT"
      , default: guard (not _pure) Just "nix-prefetch-git"
      , parse: envString
      }
      obj
  pure
    { debug: _debug
    , pure: _pure
    , dhallToJson: _dhallToJson
    , nixFormat: _nixFormat
    , nixPrefetchGit: _nixPrefetchGit
    }

parseEnvDebug :: Object String -> Boolean
parseEnvDebug obj =
  lookupEnv
    { name: "DEBUG"
    , default: Just false
    , parse: envBool
    }
    obj
    # either (const false) identity

cliParserInfo :: ParserInfo { | CliArgs () }
cliParserInfo =
  info (parseCliArgs <**> helper)
    ( fullDesc
        <> progDesc "Generate a nix specific lock file from all packages of `packages.dhall`"
        <> header "spago2nix-ree"
    )

-- ENV TYPES
envBool :: String -> Either ErrorStack Boolean
envBool = case _ of
  "true" -> Right true
  "false" -> Right false
  _ -> Left [ "Invalid boolean." ]

envString :: String -> Either ErrorStack String
envString = Right

-- UTIL
lookupEnv ::
  forall a.
  { name :: String
  , default :: Maybe a
  , parse :: String -> Either ErrorStack a
  } ->
  Object String -> Either ErrorStack a
lookupEnv { name, default, parse } obj = case Object.lookup name obj, default of
  Just value, _ -> parse value
  Nothing, Just default' -> Right default'
  _, _ -> Left [ "Expected " <> tick name <> "." ]
