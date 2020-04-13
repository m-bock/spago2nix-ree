module Spago2Nix.IO (runCli) where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, withExceptT)
import Control.Parallel (class Parallel, parTraverse)
import Data.Array (cons)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as Codec
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process as Node
import Options.Applicative (execParser)
import Pathy as Pathy
import Pathy.Extra.Unsandboxed (printAnyFile)
import Record (union)
import SimpleText (SimpleText)
import SimpleText as SimpleText
import Spago2Nix.Common (ErrorStack, NixPrefetchGitResult, decode, decodeJson, joinSpaces, joinStrings, jsonParser, stringifyPretty, tick)
import Spago2Nix.Config (CliArgs, Config, EnvVars, cliParserInfo, parseEnvVars)
import Spago2Nix.Config.IO as Config.IO
import Spago2Nix.Data.PackagesLock (PackagesLock, codecPackagesLock)
import Spago2nix.Data.PackagesDhall (PackagesDhall, codecPackagesDhall)
import Sunde as Sunde

data CliState
  = CliState_Idle
  | CliState_GetConfig
  | CliState_ReadInput { path :: String }
  | CliState_NixPrefetchChunk { index :: Int, chunkSize :: Int, length :: Int }
  | CliState_Format
  | CliState_WriteOutput { path :: String }
  | CliState_Done

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

getPackagesDhall :: Config -> ExceptT ErrorStack Aff PackagesDhall
getPackagesDhall config =
  let
    fileStr = printAnyFile Pathy.posixPrinter config.target

    errorMsg _ =
      SimpleText.Sentence
        $ SimpleText.Texts
            [ SimpleText.Text "Cannot read packages file at"
            , SimpleText.Backtick $ SimpleText.Text $ fileStr
            ]
  in
    dhallToJson config
      ("./" <> fileStr)
      # (mapExceptT <<< map <<< bindFlipped)
          (jsonParser >=> decode codecPackagesDhall)
      # withExceptT (cons $ SimpleText.print $ errorMsg unit)

writePackagesLock :: Config -> PackagesLock -> ExceptT ErrorStack Aff Unit
writePackagesLock config packagesLock =
  packagesLock
    # Codec.encode codecPackagesLock
    # stringifyPretty 2
    # writeTextFile
        (printAnyFile Pathy.posixPrinter config.target)

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
    setCliState CliState_GetConfig
      *> Config.IO.getConfig
  packagesDhall <-
    withCliState (CliState_ReadInput { path: printAnyFile Pathy.posixPrinter config.packagesDhall })
      (getPackagesDhall config)
  let
    length = Map.size packagesDhall

    chunkSize = 20
  packagesLock <-
    packagesDhall
      # (Map.toUnfoldable :: _ -> Array _)
      # chunks chunkSize
      # traverseWithIndex
          ( \chunkIndex keyValue ->
              withCliState (CliState_NixPrefetchChunk { chunkSize, index: chunkIndex, length })
                ( keyValue
                    # parTraverse handleLocation
                )
          )
      <#> join
      <#> Map.fromFoldable
  withCliState
    (CliState_WriteOutput { path: printAnyFile Pathy.posixPrinter config.target })
    (writePackagesLock config packagesLock)

handleLocation (key /\ spagoPackage) = do
      nixPrefetchGitResult <-
        nixPrefetchGit config
          { repo: spagoPackage.repo
          , rev: spagoPackage.version
          }
      let
        value =
          spagoPackage
            `union`
              { rev: nixPrefetchGitResult.rev
              , nixSha256: nixPrefetchGitResult.sha256
              , name: key
              }
      pure $ key /\ value
  )

parTraverse' :: forall m a b f. Parallel f m => { max :: Int } -> (a -> m b) -> Array a -> m (Array b)
parTraverse' { max } f xs =
  chunks max xs
    <#> parTraverse f
    # sequence
    <#> join

-- UTIL
printCliState :: CliState -> Maybe String
printCliState = case _ of
  CliState_Idle -> Just "done\n"
  CliState_GetConfig -> Nothing
  CliState_ReadInput { path } ->
    Just
      $ joinSpaces
          [ "Reading"
          , tick path
          ]
  CliState_NixPrefetchChunk { index, chunkSize, length } ->
    Just
      $ joinSpaces
          [ "fetching chunk"
          , joinStrings
              [ show (index * chunkSize)
              , "-"
              , show $ min ((index + 1) * chunkSize) length
              , "/"
              , show length
              ]
          , "..."
          ]
  CliState_Format -> Just "Format result"
  CliState_WriteOutput { path } ->
    Just
      $ joinSpaces
          [ "Writing to"
          , tick path
          ]
  CliState_Done -> Just "done"

chunks :: forall a. Int -> Array a -> Array (Array a)
chunks n [] = []

chunks n xs = [ Array.take n xs ] <> chunks n (Array.drop n xs)
