module Options.Applicative.Compat.Pathy where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either(..), either)
import Data.Lens (Iso')
import Data.Maybe (Maybe(..))
import Options.Applicative (ReadM)
import Options.Applicative as Options.Applicative
import Pathy (class IsDirOrFile, class IsRelOrAbs, AbsFile, AnyFile, Path, RelFile)
import Pathy as Pathy
import SimpleText as SimpleText

readAbsFile :: Pathy.Parser -> ReadM AbsFile
readAbsFile parser =
  let
    errorMsg _ =
      SimpleText.Sentence
        $ SimpleText.Text "Cannot parse absolute file path"
  in
    do
      result <- Options.Applicative.str
      case Pathy.parseAbsFile parser result of
        Nothing -> Options.Applicative.readerError $ SimpleText.print $ errorMsg unit
        Just x -> pure x

readRelFile :: Pathy.Parser -> ReadM RelFile
readRelFile parser =
  let
    errorMsg _ =
      SimpleText.Sentence
        $ SimpleText.Text "Cannot parse relative file path"
  in
    do
      result <- Options.Applicative.str
      case Pathy.parseRelFile parser result of
        Nothing -> Options.Applicative.readerError $ SimpleText.print $ errorMsg unit
        Just x -> pure x

readAnyFile :: Pathy.Parser -> ReadM AnyFile
readAnyFile parser = (Left <$> readAbsFile parser) <|> (Right <$> readRelFile parser)

showDefaultAnyFile :: Pathy.Printer -> AnyFile -> String
showDefaultAnyFile printer = either (showDefaultPath printer) (showDefaultPath printer)

showDefaultPath :: forall a b. IsRelOrAbs a => IsDirOrFile b => Pathy.Printer -> Path a b -> String
showDefaultPath printer = Pathy.sandboxAny >>> Pathy.printPath printer
