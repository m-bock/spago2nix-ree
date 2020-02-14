module Spago2Nix.IO (runCli) where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, withExceptT)
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import NixAST as NixAST
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Node
import Options.Applicative (execParser)
import Record (union)
import Spago2Nix.Common (ErrorStack, NixPrefetchGitResult, decodeJson, decodeMapFromObject, joinNl, joinSpaces, joinStrings, jsonParser, tick)
import Spago2Nix.Config (CliArgs, Config, EnvVars, cliParserInfo, parseEnvVars)
import Spago2Nix.SpagoPackage (SpagoPackage)
import Spago2Nix.SpagoPackage as SpagoPackage
import Sunde as Sunde

data CliState
  = CliState_Idle
  | CliState_GetConfig
  | CliState_ReadInput { path :: String }
  | CliState_NixPrefetch { index :: Int, length :: Int, spagoPackage :: SpagoPackage }
  | CliState_Format
  | CliState_WriteOutput { path :: String }
  | CliState_Done

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

getConfig :: ExceptT ErrorStack Aff Config
getConfig = union <$> getCliArgs <*> getEnvVars

spawn ::
  { cmd :: String, args :: Array String, stdin :: Maybe String } ->
  ExceptT ErrorStack Aff String
spawn options =
  -- TODO: Check why error is not catched
  Sunde.spawn options defaultSpawnOptions
    # try
    <#> case _ of
        Right { exit: Normally 0, stdout } -> Right stdout
        Right { stderr } -> Left [ stderr ]
        Left error -> Left [ "Command not found. " <> tick options.cmd ]
    <#> lmap (cons "spawn")
    # ExceptT

dhallToJson ::
  forall cfg.
  { dhallToJson :: String | cfg } -> String -> ExceptT ErrorStack Aff String
dhallToJson config dhallCode =
  spawn
    { cmd: config.dhallToJson
    , args: []
    , stdin: Just dhallCode
    }
    # withExceptT (cons "dhallToJson")

nixPrefetchGit ::
  forall cfg.
  { nixPrefetchGit :: String | cfg } ->
  { repo :: String, rev :: String } -> ExceptT ErrorStack Aff NixPrefetchGitResult
nixPrefetchGit config { repo, rev } =
  spawn
    { cmd: config.nixPrefetchGit
    , args: [ repo, "--rev", rev ]
    , stdin: Nothing
    }
    # (mapExceptT <<< map <<< bindFlipped) (jsonParser >=> decodeJson)
    # withExceptT (cons "nixPrefetchGit")

nixFormat ::
  forall cfg.
  { nixFormat :: String | cfg } ->
  { source :: String } -> ExceptT ErrorStack Aff String
nixFormat config options =
  spawn
    { cmd: config.nixFormat
    , args: []
    , stdin: Just options.source
    }
    # withExceptT (cons "nixFormat")

getSpagoPackages :: Config -> ExceptT ErrorStack Aff (Map String SpagoPackage)
getSpagoPackages config =
  dhallToJson config
    ("./" <> config.spagoPackages)
    # (mapExceptT <<< map <<< bindFlipped)
        (jsonParser >=> decodeMapFromObject decodeJson)
    # withExceptT (cons $ "Read spago config at " <> tick config.spagoPackages <> ".")

writeTextFile :: String -> String -> ExceptT ErrorStack Aff Unit
writeTextFile path content =
  try (FS.writeTextFile UTF8 path content)
    # ExceptT
    # withExceptT (const [ "Cannot write to file " <> tick path <> "." ])

setCliState :: CliState -> ExceptT ErrorStack Aff Unit
setCliState cliState = case printCliState cliState of
  Just output -> log output
  Nothing -> pure unit

withCliState :: forall a. CliState -> ExceptT ErrorStack Aff a -> ExceptT ErrorStack Aff a
withCliState cliState m = do
  setCliState cliState
  result <- m
  setCliState CliState_Idle
  pure result

runCli :: ExceptT ErrorStack Aff Unit
runCli = do
  config <-
    withCliState
      CliState_GetConfig
      getConfig
  spagoPackages <-
    withCliState
      (CliState_ReadInput { path: config.spagoPackages })
      (getSpagoPackages config)
  let
    length = Map.size spagoPackages
  spagoPackages
    # (Map.toUnfoldable :: _ -> Array _)
    # traverseWithIndex
        ( \index (key /\ spagoPackage) ->
            withCliState
              (CliState_NixPrefetch { index, length, spagoPackage })
              ( do
                  git <-
                    nixPrefetchGit config
                      { repo: spagoPackage.repo
                      , rev: spagoPackage.version
                      }
                  let
                    value = spagoPackage `union` { git }
                  pure $ key /\ value
              )
        )
    >>= identity
        ( \spagoPackagesEnriched ->
            withCliState
              CliState_Format
              ( spagoPackagesEnriched
                  # SpagoPackage.toNix
                  # NixAST.print
                  # (\source -> nixFormat config { source })
              )
        )
    >>= identity
        ( \result ->
            withCliState
              (CliState_WriteOutput { path: config.target })
              (writeTextFile config.target result)
        )
    >>= (const $ setCliState CliState_Done)

-- UTIL
printCliState :: CliState -> Maybe String
printCliState = case _ of
  -- TODO: Remove Maybe
  CliState_Idle -> Just "done\n"
  CliState_GetConfig -> Just "Get config"
  CliState_ReadInput { path } ->
    Just
      $ joinSpaces
          [ "Reading"
          , tick path
          ]
  CliState_NixPrefetch { index, length, spagoPackage } ->
    Just
      $ joinNl
          [ joinStrings
              [ show (index + 1)
              , "/"
              , show length
              ]
          , joinSpaces [ "fetching", tick spagoPackage.repo, "..." ]
          ]
  CliState_Format -> Just "Format result"
  CliState_WriteOutput { path } ->
    Just
      $ joinSpaces
          [ "Writing to"
          , tick path
          ]
  CliState_Done -> Just "done"
