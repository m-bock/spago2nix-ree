module Main where

import Prelude
import Control.Bind (bindFlipped)
import Control.Monad.Except (ExceptT(..), lift, mapExceptT, runExceptT, withExceptT)
import Data.Argonaut (class DecodeJson, decodeJson, jsonParser)
import Data.Array (cons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, runAff_, try)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import FormatNix as Nix
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Node.Process as Node
import Node.Process as Process
import Options.Applicative (Parser, ParserInfo, execParser, fullDesc, header, help, helper, info, long, metavar, progDesc, showDefault, strOption, value, (<**>))
import Partial.Unsafe (unsafeCrashWith)
import Sunde as Sunde

type Env
  = { dhallToJson :: String
    }

type CliArgs
  = { spagoConfig :: String
    , target :: String
    }

type Config
  = { env :: Env, args :: CliArgs }

parseCliArgs :: Parser CliArgs
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

opts :: ParserInfo CliArgs
opts =
  info (parseCliArgs <**> helper)
    ( fullDesc
        <> progDesc ""
        <> header "spago.dhall2nix - Generate nix expressions from spago config files"
    )

newtype SpagoConfig
  = SpagoDhall
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
  decodeJson = unsafeCrashWith ""

generateNix :: SpagoConfig -> Nix.Expr
generateNix = \_ -> unsafeCrashWith ""

type ErrorStack
  = Array String

defaultEnv :: Env
defaultEnv = { dhallToJson: "dhall-to-json" }

-- IO
--
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
        Left error -> Left [ "Command not found", show error ]
    <#> lmap (cons "dhallToJson")
    # ExceptT

lookupEnv :: String -> ExceptT ErrorStack Aff String
lookupEnv env =
  Node.lookupEnv env
    # liftEffect
    <#> note [ "Environment variable not found.", env ]
    # ExceptT

getEnv :: ExceptT ErrorStack Aff Env
getEnv =
  lookupEnv "CONFIG"
    # (mapExceptT <<< map) case _ of
        Left _ -> Right defaultEnv
        Right value -> decodeJsonFromString value
    # withExceptT (cons "Read Config")

getCliArgs :: ExceptT ErrorStack Aff CliArgs
getCliArgs =
  execParser opts
    # liftEffect
    # lift

getSpagoConfig :: Config -> ExceptT ErrorStack Aff SpagoConfig
getSpagoConfig config =
  dhallToJson config.env.dhallToJson
    ("./" <> config.args.spagoConfig)
    # (mapExceptT <<< map <<< bindFlipped) decodeJsonFromString

writeTextFile :: String -> String -> ExceptT ErrorStack Aff Unit
writeTextFile = unsafeCrashWith ""

-- MAIN
--
main' :: ExceptT ErrorStack Aff Unit
main' = do
  env <- getEnv
  args <- getCliArgs
  let
    config = { env, args }
  spagoConfig <- getSpagoConfig config
  let
    nix = generateNix spagoConfig
  _ <- writeTextFile args.target (Nix.printExpr nix)
  pure unit

main :: Effect Unit
main =
  runExceptT main'
    # runAff_ case _ of
        Left unhandledError -> do
          Console.error "Unknown error."
          Process.exit 1
        Right (Left errorStack) -> do
          Console.error $ printErrorStack errorStack
          Process.exit 1
        Right (Right _) -> pure unit

-- UTIL
--
decodeJsonFromString :: forall a. DecodeJson a => String -> Either ErrorStack a
decodeJsonFromString =
  (>=>)
    (jsonParser >>> lmap (pure >>> cons "Invalid JSON"))
    (decodeJson >>> lmap (pure >>> cons "Invalid Structure"))

printErrorStack :: ErrorStack -> String
printErrorStack errorStack = String.joinWith "\n" $ cons "Error:" errorStack
