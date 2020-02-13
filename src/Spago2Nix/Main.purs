module Spago2Nix.Main (main) where

import Prelude
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (runAff_)
import Effect.Class.Console as Console
import Node.Process as Node
import Node.Process as Process
import Spago2Nix.Common (nativeErrorToStack, printErrorStack)
import Spago2Nix.Config (parseEnvDebug)
import Spago2Nix.IO (runCli) as IO

main :: Effect Unit
main = do
  debug <- Node.getEnv <#> parseEnvDebug
  runExceptT IO.runCli
    # runAff_ case _ of
        Left unknownError -> do
          nativeErrorToStack debug unknownError
            # printErrorStack
            # Console.error
          Process.exit 1
        Right (Left errorStack) -> do
          Console.error $ printErrorStack errorStack
          Process.exit 1
        Right (Right _) -> pure unit
