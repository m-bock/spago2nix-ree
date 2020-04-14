module Spago2Nix.Data.Codec.Argonaut.Compat.Pathy.Unsandboxed
  ( PathyConfig
  , codecAbsDir
  , codecRelDir
  , codecAnyDir
  , codecAbsFile
  , codecRelFile
  , codecAnyFile
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.Codec.Argonaut (JsonCodec, decode, encode, json, prismaticCodec)
import Data.Codec.Argonaut as Codec
import Data.Either (Either(..))
import Data.Either as Either
import Data.Maybe (Maybe)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, AnyDir, Dir, File, Path, Rel, AnyFile)
import Pathy as Pathy

type PathyConfig
  = { printer :: Pathy.Printer
    , parser :: Pathy.Parser
    }

codecAbsDir :: PathyConfig -> JsonCodec (Path Abs Dir)
codecAbsDir { printer, parser } =
  prismaticCodec decoder encoder
    $ Codec.string
  where
  decoder :: String -> Maybe (Path Abs Dir)
  decoder = Pathy.parseAbsDir parser

  encoder :: (Path Abs Dir) -> String
  encoder = printAny printer

codecAbsFile :: PathyConfig -> JsonCodec (Path Abs File)
codecAbsFile { printer, parser } =
  prismaticCodec decoder encoder
    $ Codec.string
  where
  decoder :: String -> Maybe (Path Abs File)
  decoder = Pathy.parseAbsFile parser

  encoder :: (Path Abs File) -> String
  encoder = printAny printer

codecRelDir :: PathyConfig -> JsonCodec (Path Rel Dir)
codecRelDir { printer, parser } =
  prismaticCodec decoder encoder
    $ Codec.string
  where
  decoder :: String -> Maybe (Path Rel Dir)
  decoder = Pathy.parseRelDir parser

  encoder :: Path Rel Dir -> String
  encoder = printAny printer

codecRelFile :: PathyConfig -> JsonCodec (Path Rel File)
codecRelFile { printer, parser } =
  prismaticCodec decoder encoder
    $ Codec.string
  where
  decoder :: String -> Maybe (Path Rel File)
  decoder = Pathy.parseRelFile parser

  encoder :: Path Rel File -> String
  encoder = printAny printer

codecAnyDir :: PathyConfig -> JsonCodec AnyDir
codecAnyDir pathyConfig = prismaticCodec fromJson toJson json
  where
  fromJson x =
    (decode (codecAbsDir pathyConfig) x <#> Left)
      <|> (decode (codecRelDir pathyConfig) x <#> Right)
      # Either.hush

  toJson = case _ of
    Left x -> encode (codecAbsDir pathyConfig) x
    Right x -> encode (codecRelDir pathyConfig) x

codecAnyFile :: PathyConfig -> JsonCodec AnyFile
codecAnyFile pathyConfig = prismaticCodec fromJson toJson json
  where
  fromJson x =
    (decode (codecAbsFile pathyConfig) x <#> Left)
      <|> (decode (codecRelFile pathyConfig) x <#> Right)
      # Either.hush

  toJson = case _ of
    Left x -> encode (codecAbsFile pathyConfig) x
    Right x -> encode (codecRelFile pathyConfig) x

printAny :: forall a b. IsRelOrAbs a => IsDirOrFile b => Pathy.Printer -> Path a b -> String
printAny printer = Pathy.sandboxAny >>> Pathy.printPath printer
