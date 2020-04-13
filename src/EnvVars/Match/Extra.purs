module EnvVars.Match.Extra where

import Prelude
import Control.Alt ((<|>))
import Data.Either (Either(..))
import Data.Either as Either
import EnvVars.Match as EnvVars.Match
import Pathy (AbsFile, RelFile, AnyFile)
import Pathy as Pathy
import SimpleText as SimpleText

absFile :: String -> Either String AbsFile
absFile x =
  let
    errorMsg = SimpleText.print $ SimpleText.Sentence $ SimpleText.Text "Cannot parse absolute path"
  in
    x
      # EnvVars.Match.string
      >>= (Pathy.parseAbsFile Pathy.posixParser >>> Either.note errorMsg)

relFile :: String -> Either String RelFile
relFile x =
  let
    errorMsg = SimpleText.print $ SimpleText.Sentence $ SimpleText.Text "Cannot parse relative path"
  in
    x
      # EnvVars.Match.string
      >>= (Pathy.parseRelFile Pathy.posixParser >>> Either.note errorMsg)

anyFile :: String -> Either String AnyFile
anyFile x = (Left <$> absFile x) <|> (Right <$> relFile x)
