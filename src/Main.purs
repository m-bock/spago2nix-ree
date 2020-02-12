module Main where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT, withExceptT)
import Data.Argonaut (class DecodeJson, Json, decodeJson, jsonParser, toObject, (.:))
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), either, note)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.String as String
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, runAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object (Object, keys, lookup)
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Process (lookupEnv)
import Node.Process as Node
import Node.Process as Process
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, showDefault, strOption, value, (<**>))
import Partial.Unsafe (unsafeCrashWith)
import Record (union)
import Sunde as Sunde

type Env r
  = ( debug :: Boolean
    , dhallToJson :: String
    , nixFormat :: String
    , nixPrefetchGit :: String
    | r
    )

type CliArgs r
  = ( spagoConfig :: String
    , target :: String
    | r
    )

type Config
  = { | Env (CliArgs ()) }

parseCliArgs :: Parser { | CliArgs () }
parseCliArgs =
  (\spagoConfig target -> { spagoConfig, target })
    <$> strOption
        ( long "spagoConfig"
            <> metavar "SPAGO_CONFIG"
            <> help "path to spago.dhall"
            <> showDefault
            <> value "spago.dhall"
        )
    <*> strOption
        ( long "target"
            <> metavar "TARGET"
            <> help "path to spago.nix"
            <> showDefault
            <> value "spago.nix"
        )

parseEnv :: Object String -> Either ErrorStack { | Env () }
parseEnv obj = ado
  debug <-
    lookupOptional "DEBUG" false parseEnvBool obj
  dhallToJson <-
    lookupOptional "DHALL_TO_JSON" "dhall-to-json" parseEnvString obj
  nixFormat <-
    lookupOptional "NIX_FORMAT" "nixfmt" parseEnvString obj
  nixPrefetchGit <-
    lookupOptional "NIX_PREFETCH_GIT" "nix-prefetch-git" parseEnvString obj
  in { debug, dhallToJson, nixFormat, nixPrefetchGit }
  where
  lookup' key obj = lookup key obj # note [ "Expected `" <> key <> "`." ]

lookupOptional ::
  forall a.
  String -> a -> (String -> Either ErrorStack a) -> Object String -> Either ErrorStack a
lookupOptional key def cont obj = case lookup key obj of
  Just value -> cont value # lmap (cons ("Read `" <> key <> "`."))
  Nothing -> Right def

parseEnvBool :: String -> Either ErrorStack Boolean
parseEnvBool = case _ of
  "true" -> Right true
  "false" -> Right false
  _ -> Left [ "Invalid boolean." ]

parseEnvString :: String -> Either ErrorStack String
parseEnvString = Right

parseEnvDebug :: Object String -> Boolean
parseEnvDebug obj =
  lookup "DEBUG" obj # note []
    >>= parseEnvBool
    # either (const false) identity

opts :: ParserInfo { | CliArgs () }
opts =
  info (parseCliArgs <**> helper)
    ( fullDesc
        <> progDesc ""
        <> header "spago.dhall2nix - Generate nix expressions from spago config files"
    )

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
    _obj <- toObject json # note "Expexted an object."
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
      lookup key obj # note ("Expected key `" <> key <> "`.") >>= decodeA <#> (\v -> Tuple key v)
  pure $ Map.fromFoldable pairs

data NixExpr

printNixExpr :: NixExpr -> String
printNixExpr = \_ -> unsafeCrashWith "printNixExpr"

generateNix :: SpagoConfig -> NixExpr
generateNix = \_ -> unsafeCrashWith "generateNix"

type ErrorStack
  = Array String

-- IO 
--
getEnv :: ExceptT ErrorStack Aff { | Env () }
getEnv =
  Node.getEnv
    # liftEffect
    <#> (parseEnv >>> lmap (cons "Read Environment variables."))
    # ExceptT

getCliArgs :: ExceptT ErrorStack Aff { | CliArgs () }
getCliArgs =
  execParser opts
    # liftEffect
    # lift

dhallToJson :: String -> String -> ExceptT ErrorStack Aff String
dhallToJson cmd dhallCode =
  -- TODO: Check why error is not catched
  ( try
      $ Sunde.spawn
          { cmd
          , args: []
          , stdin: Just dhallCode
          }
          defaultSpawnOptions
  )
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
    # withExceptT (cons "Read spago config.")

writeTextFile :: String -> String -> ExceptT ErrorStack Aff Unit
writeTextFile path content =
  try (FS.writeTextFile UTF8 path content)
    # ExceptT
    # withExceptT (const $ [ "Cannot write to file:", path ])

-- UTIL
--
decodeJsonFromString :: forall a. DecodeJson a => String -> Either ErrorStack a
decodeJsonFromString =
  (>=>)
    (jsonParser >>> lmap (pure >>> cons "Invalid JSON"))
    (decodeJson >>> lmap (pure >>> cons "Invalid Structure"))

printErrorStack :: ErrorStack -> String
printErrorStack errorStack =
  ([ "Something went wrong along the way..." ] <> (errorStack <#> (" - " <> _)))
    # String.joinWith "\n\n"

-- MAIN
--
run :: ExceptT ErrorStack Aff Unit
run = do
  config <- union <$> getCliArgs <*> getEnv
  spagoConfig <- getSpagoConfig config
  let
    nix = generateNix spagoConfig
  _ <- writeTextFile config.target $ printNixExpr nix
  pure unit

main :: Effect Unit
main = do
  debug <- Node.getEnv <#> parseEnvDebug
  runExceptT run
    # runAff_ case _ of
        Left unknownError -> do
          Console.error
            $ printErrorStack
                ( [ "Unknown error." ]
                    <> guard debug [ show unknownError ]
                )
          Process.exit 1
        Right (Left errorStack) -> do
          Console.error $ printErrorStack errorStack
          Process.exit 1
        Right (Right _) -> pure unit
