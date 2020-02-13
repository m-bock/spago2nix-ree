module Spago2Nix.IO (runCli) where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, withExceptT)
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import NixAST as NixAST
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Node
import Options.Applicative (execParser)
import Record (union)
import Spago2Nix.Common (ErrorStack, decodeJsonFromString, tick)
import Spago2Nix.Config (CliArgs, Config, EnvVars, cliParserInfo, parseEnvVars)
import Spago2Nix.SpagoConfig (SpagoConfig)
import Spago2Nix.SpagoConfig as SpagoConfig
import Sunde as Sunde

getEnvVars :: ExceptT ErrorStack Aff { | EnvVars () }
getEnvVars =
  Node.getEnv
    # liftEffect
    <#> (parseEnvVars >>> lmap (cons "Read Environment variables."))
    # ExceptT

getCliArgs :: ExceptT ErrorStack Aff { | CliArgs () }
getCliArgs =
  execParser cliParserInfo
    # liftEffect
    # lift

dhallToJson :: String -> String -> ExceptT ErrorStack Aff String
dhallToJson cmd dhallCode =
  -- TODO: Check why error is not catched
  Sunde.spawn
    { cmd
    , args: []
    , stdin: Just dhallCode
    }
    defaultSpawnOptions
    # try
    <#> case _ of
        Right { exit: Normally 0, stdout } -> Right stdout
        Right { stderr } -> Left [ stderr ]
        Left error -> Left [ "Command not found.", cmd ]
    <#> lmap (cons "dhallToJson")
    # ExceptT

getSpagoConfig :: Config -> ExceptT ErrorStack Aff SpagoConfig
getSpagoConfig config =
  dhallToJson config.dhallToJson
    ("./" <> config.spagoConfig)
    # (mapExceptT <<< map <<< bindFlipped) decodeJsonFromString
    # withExceptT (cons $ "Read spago config at " <> tick config.spagoConfig <> ".")

writeTextFile :: String -> String -> ExceptT ErrorStack Aff Unit
writeTextFile path content =
  try (FS.writeTextFile UTF8 path content)
    # ExceptT
    # withExceptT (const [ "Cannot write to file " <> tick path <> "." ])

runCli :: ExceptT ErrorStack Aff Unit
runCli = do
  config <- union <$> getCliArgs <*> getEnvVars
  spagoConfig <- getSpagoConfig config
  let
    nix = SpagoConfig.toNix spagoConfig
  _ <- writeTextFile config.target $ NixAST.print nix
  pure unit
