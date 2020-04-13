module Pathy.Extra.Unsandboxed where

import Prelude
import Data.Either (either)
import Pathy (class IsDirOrFile, class IsRelOrAbs, AnyFile, Path)
import Pathy as Pathy

printPath :: forall a b. IsRelOrAbs a => IsDirOrFile b => Pathy.Printer -> Path a b -> String
printPath printer = Pathy.sandboxAny >>> Pathy.printPath printer

printAnyFile :: Pathy.Printer -> AnyFile -> String
printAnyFile printer = either (printPath printer) (printPath printer)
