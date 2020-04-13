module Spago2Nix.Config.IO where

import Prelude
import Control.Monad.Except (ExceptT(..), lift, withExceptT)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import EnvVars as EnvVars
import Options.Applicative (execParser)
import Record as Record
import Spago2Nix.Common (ErrorStack)
import Spago2Nix.Config (CliArgs, Config, cliParserInfo, parseEnvVars)

getCliArgs :: ExceptT ErrorStack Aff { | CliArgs () }
getCliArgs =
  execParser cliParserInfo
    # liftEffect
    # lift

getConfig :: ExceptT ErrorStack Aff Config
getConfig = do
  envVars <-
    parseEnvVars
      # EnvVars.getEnvVars
      # liftEffect
      # ExceptT
      # withExceptT pure
  cliArgs <- getCliArgs
  pure $ envVars `Record.union` cliArgs
