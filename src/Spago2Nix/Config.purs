module Spago2Nix.Config
  ( Config
  , EnvVars
  , CliArgs
  , parseEnvDebug
  , parseEnvVars
  , cliParserInfo
  ) where

import Prelude
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Symbol (SProxy(..))
import EnvVars as EnvVars
import EnvVars.Match as EnvVars.Match
import Foreign.Object (Object)
import Options.Applicative (Parser, ParserInfo, fullDesc, header, help, helper, info, long, metavar, option, progDesc, showDefaultWith, value, (<**>))
import Options.Applicative.Compat.Pathy (readAnyFile, showDefaultAnyFile)
import Pathy (AnyFile, (</>))
import Pathy as Pathy

type Config
  = { | EnvVars (CliArgs ()) }

type EnvVars r
  = ( debug :: Boolean
    , pure :: Boolean
    , dhallToJson :: String
    , nixPrefetchGit :: String
    | r
    )

type CliArgs r
  = ( packagesDhall :: AnyFile
    , target :: AnyFile
    | r
    )

parseCliArgs :: Parser { | CliArgs () }
parseCliArgs =
  { packagesDhall: _, target: _ }
    <$> option (readAnyFile Pathy.posixParser)
        ( long "packagesDhall"
            <> metavar "PACKAGES_DHALL"
            <> help "Path to spago packages file"
            <> showDefaultWith (showDefaultAnyFile Pathy.posixPrinter)
            <> value
                ( Right $ Pathy.currentDir </> Pathy.file (SProxy :: SProxy "packages.dhall")
                )
        )
    <*> ( option (readAnyFile Pathy.posixParser)
          ( long "target"
              <> metavar "TARGET"
              <> help "Path to target file"
              <> showDefaultWith (showDefaultAnyFile Pathy.posixPrinter)
              <> value
                  ( Right $ Pathy.currentDir </> Pathy.file (SProxy :: SProxy "spago-lock.json")
                  )
          )
      )

parseEnvVars :: Object String -> Either String { | EnvVars () }
parseEnvVars obj = do
  _debug <-
    EnvVars.lookupEnv
      { name: "DEBUG"
      , default: Just false
      , parse: EnvVars.Match.boolean
      }
      obj
  _pure <-
    EnvVars.lookupEnv
      { name: "PURE"
      , default: Just false
      , parse: EnvVars.Match.boolean
      }
      obj
  _dhallToJson <-
    EnvVars.lookupEnv
      { name: "DHALL_TO_JSON"
      , default: guard (not _pure) Just "dhall-to-json"
      , parse: EnvVars.Match.string
      }
      obj
  _nixPrefetchGit <-
    EnvVars.lookupEnv
      { name: "NIX_PREFETCH_GIT"
      , default: guard (not _pure) Just "nix-prefetch-git"
      , parse: EnvVars.Match.string
      }
      obj
  pure
    { debug: _debug
    , pure: _pure
    , dhallToJson: _dhallToJson
    , nixPrefetchGit: _nixPrefetchGit
    }

parseEnvDebug :: Object String -> Boolean
parseEnvDebug obj =
  EnvVars.lookupEnv
    { name: "DEBUG"
    , default: Just false
    , parse: EnvVars.Match.boolean
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
