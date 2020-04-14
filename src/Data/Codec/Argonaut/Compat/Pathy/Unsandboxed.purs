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
import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Common as Codec.Common
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
codecAnyDir pathyConfig = Codec.Common.either (codecAbsDir pathyConfig) (codecRelDir pathyConfig)

codecAnyFile :: PathyConfig -> JsonCodec AnyFile
codecAnyFile pathyConfig = Codec.Common.either (codecAbsFile pathyConfig) (codecRelFile pathyConfig)

printAny :: forall a b. IsRelOrAbs a => IsDirOrFile b => Pathy.Printer -> Path a b -> String
printAny printer = Pathy.sandboxAny >>> Pathy.printPath printer
