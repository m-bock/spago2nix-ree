module Spago2Nix.Data.Codec.Argonaut.Compat.Pathy.Unsandboxed
  ( PathyConfig
  , codecAbsDir
  , codecRelDir
  , codecAnyDir
  ) where

import Prelude
import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut as Codec
import Data.Codec.Argonaut.Common as Codec.Common
import Data.Maybe (Maybe)
import Pathy (class IsDirOrFile, class IsRelOrAbs, Abs, AnyDir, Dir, Path, Rel)
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

codecRelDir :: PathyConfig -> JsonCodec (Path Rel Dir)
codecRelDir { printer, parser } =
  prismaticCodec decoder encoder
    $ Codec.string
  where
  decoder :: String -> Maybe (Path Rel Dir)
  decoder = Pathy.parseRelDir parser

  encoder :: Path Rel Dir -> String
  encoder = printAny printer

codecAnyDir :: PathyConfig -> JsonCodec AnyDir
codecAnyDir pathyConfig = Codec.Common.either (codecAbsDir pathyConfig) (codecRelDir pathyConfig)

printAny :: forall a b. IsRelOrAbs a => IsDirOrFile b => Pathy.Printer -> Path a b -> String
printAny printer = Pathy.sandboxAny >>> Pathy.printPath printer
