module Spago2Nix.Data.URI where

import Prelude
import Data.Codec.Argonaut (JsonCodec, prismaticCodec)
import Data.Codec.Argonaut as Codec
import Data.Either as Either
import Data.Maybe (Maybe)
import Text.Parsing.Parser as Parsing
import URI (Query)
import URI.AbsoluteURI (AbsoluteURI, AbsoluteURIOptions, HierPath, Host, Path, Port, UserInfo)
import URI.AbsoluteURI as AbsoluteURI
import URI.HostPortPair (HostPortPair)
import URI.HostPortPair as HostPortPair

type URI
  = AbsoluteURI UserInfo (HostPortPair Host Port) Path HierPath Query

options ∷ Record (AbsoluteURIOptions UserInfo (HostPortPair Host Port) Path HierPath Query)
options =
  { parseUserInfo: pure
  , printUserInfo: identity
  , parseHosts: HostPortPair.parser pure pure
  , printHosts: HostPortPair.print identity identity
  , parsePath: pure
  , printPath: identity
  , parseHierPath: pure
  , printHierPath: identity
  , parseQuery: pure
  , printQuery: identity
  }

codec :: JsonCodec URI
codec =
  prismaticCodec decoder encoder
    $ Codec.string
  where
  decoder :: String -> Maybe URI
  decoder str = Parsing.runParser str parser # Either.hush

  encoder :: URI -> String
  encoder = print

print :: URI -> String
print = AbsoluteURI.print options

parser :: Parsing.Parser String URI
parser = AbsoluteURI.parser options
