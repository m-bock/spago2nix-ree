module Pathy.Extra.Unsandboxed (unsafePrintAnyFile) where

import Prelude
import Data.Either (either)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AnyFile, Path)
import Pathy as Pathy

unsafePrintAnyFile :: Pathy.Printer -> AnyFile -> String
unsafePrintAnyFile printer = either print' print'
  where
  print' :: forall a b. IsRelOrAbs a => IsDirOrFile b => Path a b -> String
  print' = Pathy.sandboxAny >>> Pathy.unsafePrintPath printer
